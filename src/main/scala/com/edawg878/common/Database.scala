package com.edawg878.common

import java.time.temporal.ChronoUnit
import java.time.{Duration, Instant}
import java.util.UUID
import java.util.logging.{Level, Logger}

import com.edawg878.common.Conversions._
import com.edawg878.common.DateUnit.Zones
import com.edawg878.common.Group.Group
import com.edawg878.common.Server.{Player, Server}
import com.edawg878.common.DateUnit.Implicits.standard
import net.md_5.bungee.api.ChatColor
import reactivemongo.api.MongoDriver
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.api.indexes.{Index, IndexType}
import reactivemongo.bson.Subtype.UuidSubtype
import reactivemongo.bson._
import com.edawg878.common.MessageFormatter._

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

case class PlayTime(firstLogin: Instant = Instant.now(),
                    lastSeen: Instant = Instant.now(),
                    amount: Duration = Duration.ZERO) {

  def login: PlayTime = copy(lastSeen = Instant.now())

  def logout: PlayTime = copy(amount = duration(online = true))

  def duration(online: Boolean = false): Duration = {
    if (online) amount.plus(activity)
    else amount
  }

  def activity: Duration = Duration.between(lastSeen, Instant.now())

}

case class PlayerData(id: UUID,
                      name: String,
                      usernames: Set[String] = Set.empty,
                      displayName: Option[String] = None,
                      group: Group = Group.Default,
                      perks: Set[String] = Set.empty,
                      tier: Int = 0,
                      plotLimit: Int = 1,
                      voteCredits: Int = 0,
                      playTime: PlayTime = PlayTime()) {

  def this(p: Player) =
    this(id = p.getUniqueId, name = p.getName)

  def displayPlayTime(online: Boolean): String = {
    val time = DateUnit.format(playTime.duration(online)).mkString
    val first = playTime.firstLogin.atZone(Zones.Default).toLocalDate.toString
    info"[$name] has played for [$time] since [$first]"
  }

  def displayTier: String = info"[$name] has tier [$tier]"

  def displayCredits: String = {
    if (voteCredits == 0) info"[$name] has no credits"
    else if (voteCredits == 1) info"[$name] has [1] credit"
    else info"[$name] has [$voteCredits] credits"
  }

  def displayPerks: String = {
    if (perks.isEmpty) info"[$name] has no perks"
    else info"[$name] has the following perks: ${perks.mkStringPretty}"
  }

  def displayGroup = info"[$name] is in group [${group.name}]"

}

class PlayerNotFound(name: String) extends RuntimeException(s"Player '$name' was not found in the database")

trait PlayerRepository {

  def find(player: Player): Future[PlayerData] =
    search(player.getUniqueId).map(_.getOrElse(throw new PlayerNotFound(player.getName)))

  def traverseById(ids: UUID*): Future[Seq[PlayerData]] = Future.traverse(ids)(find)

  def traverseByName(names: String*): Future[Seq[PlayerData]] = Future.traverse(names) { name =>
    search(name) map {
      case Nil => throw new PlayerNotFound(name)
      case seq => seq.head
    }
  }

  def traverse(players: Player*): Future[Seq[PlayerData]] = Future.traverse(players)(find)

  def find(id: UUID): Future[PlayerData] =
    search(id).map(_.getOrElse(throw new PlayerNotFound(id.toString)))

  def insert(data: PlayerData): Unit

  def search(id: UUID): Future[Option[PlayerData]]

  def search(name: String): Future[Seq[PlayerData]]

  def save(data: PlayerData): Unit

  def delete(id: UUID): Unit

}

trait BSONHandlers {

  implicit object UUIDHandler extends BSONHandler[BSONBinary, UUID] {

    override def write(id: UUID): BSONBinary = BSONBinary(toBytes(id), UuidSubtype)

    def toBytes(id: UUID): Array[Byte] = {
      val shifts = Array.range(0, 64, 8).reverse
      def split(bits: Long) =
        shifts map (bits >>> _) map (_.asInstanceOf[Byte])
      split(id.getMostSignificantBits) ++ split(id.getLeastSignificantBits)
    }

    override def read(bson: BSONBinary): UUID = fromBytes(bson.value.readArray(bson.value.size))

    def fromBytes(data: Array[Byte]): UUID = {
      @tailrec def read(r: Range, acc: Long = 0): Long = {
        if (r.head == r.last) acc
        else read(r.drop(1), (acc << 8) | (data(r.head) & 0xff))
      }
      new UUID(read(0 to 8), read(8 to 16))
    }
  }

  implicit object GroupHandler extends BSONHandler[BSONString, Group] {

    override def write(g: Group): BSONString = BSONString(g.name)

    override def read(bson: BSONString): Group = Group.withName(bson.value).get

  }

  implicit object DurationHandler extends BSONHandler[BSONLong, Duration] {

    override def write(d: Duration): BSONLong = BSONLong(d.get(ChronoUnit.SECONDS))

    override def read(bson: BSONLong): Duration = Duration.ofSeconds(bson.value)

  }

  implicit object InstantHandler extends BSONHandler[BSONDateTime, Instant] {

    override def write(time: Instant): BSONDateTime = BSONDateTime(time.toEpochMilli)

    override def read(bson: BSONDateTime): Instant = Instant.ofEpochMilli(bson.value)
  }

  implicit object PlayerDataHandler extends BSONDocumentWriter[PlayerData] with BSONDocumentReader[PlayerData] {
    override def write(self: PlayerData): BSONDocument = {
      import self._
      BSONDocument(
        "_id" -> id,
        "name" -> name,
        "lowerName" -> name.toLowerCase,
        "usernames" -> usernames.toOption,
        "displayName" -> displayName,
        "group" -> group,
        "perks" -> perks.toOption,
        "tier" -> tier,
        "plotLimit" -> plotLimit,
        "voteCredits" -> voteCredits,
        "firstLogin" -> playTime.firstLogin,
        "lastSeen" -> playTime.lastSeen,
        "playTime" -> playTime.amount
      )
    }

    override def read(doc: BSONDocument): PlayerData = {
      val _id = doc.getAs[UUID]("_id").get
      val _name = doc.getAs[String]("name").get
      val _usernames = doc.getAs[Set[String]]("usernames").getOrElse(Set.empty)
      val _perks = doc.getAs[Set[String]]("perks").getOrElse(Set.empty)
      val _displayName = doc.getAs[String]("displayName")
      val _tier = doc.getAs[Int]("tier").get
      val _plotLimit = doc.getAs[Int]("plotLimit").get
      val _voteCredits = doc.getAs[Int]("voteCredits").get
      val _group = doc.getAs[Group]("group").get
      val _firstLogin = doc.getAs[Instant]("firstLogin").get
      val _lastSeen = doc.getAs[Instant]("lastSeen").get
      val _amount = doc.getAs[Duration]("playTime").get
      val _playTime = PlayTime(firstLogin = _firstLogin, lastSeen = _lastSeen, amount = _amount)
      PlayerData(
        id = _id,
        name = _name,
        usernames = _usernames,
        perks = _perks,
        displayName = _displayName,
        tier = _tier,
        plotLimit = _plotLimit,
        voteCredits = _voteCredits,
        group = _group,
        playTime = _playTime
      )
    }
  }

}

class MongoPlayerRepository(logger: Logger) extends PlayerRepository with BSONHandlers {

  val driver = new MongoDriver
  val conn = driver.connection(List("localhost"))
  val db = conn.db("minecraft")
  val col = db.collection[BSONCollection]("players")

  def queryById(id: UUID): BSONDocument = BSONDocument("_id" -> id)

  def queryByName(name: String): BSONDocument = BSONDocument("lowerName" -> name.toLowerCase)

  private def ensureIndex(name: String, index: Index): Unit = {
    logger.info(s"Ensuring index '$name'...")
    col.indexesManager.ensure(index) onComplete {
      case Success(created) =>
        if (created) logger.info(s"Created '$name' index")
        else logger.info("Index already existed")
      case Failure(t) =>
        logger.log(Level.SEVERE, s"Failed to create index '$name'", t)
    }
  }

  def ensureIndexes(): Unit = {
    ensureIndex("Lowercase Username", Index(key = List(("lowerName", IndexType.Ascending))))
  }

  override def insert(data: PlayerData): Unit = col.insert(data)

  override def save(data: PlayerData): Unit = col.save(data)

  override def search(id: UUID): Future[Option[PlayerData]] =
    col.find(queryById(id)).cursor[PlayerData].headOption

  override def search(name: String): Future[Seq[PlayerData]] =
    col.find(queryByName(name)).cursor[PlayerData].collect[Vector]()

  override def delete(id: UUID): Unit = col.remove(queryById(id))

}