package com.edawg878.common

import java.time.{Duration, _}
import java.time.temporal.ChronoUnit
import java.util.UUID
import java.util.logging.{Level, Logger}

import com.edawg878.bukkit.plot.{Plot, PlotId, PlotWorld}
import com.edawg878.common.Server.Player
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.api.indexes.{Index, IndexType}
import reactivemongo.api.{DB, DefaultDB, MongoConnection, MongoDriver}
import reactivemongo.bson.Subtype.UuidSubtype
import reactivemongo.bson._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}


class PlayerNotFound(name: String) extends RuntimeException(s"Player '$name' was not found in the database")

trait PlayerRepository {

  def find(player: Player): Future[PlayerData] =
    search(player.id).map(_.getOrElse(throw new PlayerNotFound(player.name)))

  def traverseById(ids: Seq[UUID]): Future[Seq[PlayerData]] = Future.traverse(ids)(search).map(_.flatten)

  def traverseByName(names: Seq[String]): Future[Seq[PlayerData]] = Future.traverse(names) { name =>
    searchAll(name) map {
      case Nil => throw new PlayerNotFound(name)
      case seq => seq.head
    }
  }

  def traverse(players: Player*): Future[Seq[PlayerData]] = Future.traverse(players)(find)

  def find(id: UUID): Future[PlayerData] =
    search(id).map(_.getOrElse(throw new PlayerNotFound(id.toString)))

  def insert(data: PlayerData): Unit

  def search(id: UUID): Future[Option[PlayerData]]

  def search(name: String): Future[Option[PlayerData]] =
    searchAll(name).map(_.sorted.headOption)

  def searchAll(name: String): Future[Seq[PlayerData]]

  def find(name: String): Future[PlayerData] =
    search(name).map(_.headOption).map(_.getOrElse(throw new PlayerNotFound(name)))

  def save(data: PlayerData): Unit

  def delete(id: UUID): Unit

}

trait PlotRepository {

  def findAll(w: PlotWorld): Future[Seq[Plot]]

  def find(w: PlotWorld, id: PlotId): Future[Option[Plot]]

  def insert(w: PlotWorld, p: Plot): Unit

  def save(w: PlotWorld, p: Plot): Unit

  def delete(w: PlotWorld, p: PlotId): Unit

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
        if (r.length == 1) acc
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

  implicit object LocalDateHandler extends BSONHandler[BSONLong, LocalDate] {

    override def write(d: LocalDate): BSONLong = BSONLong(d.toEpochDay)

    override def read(bson: BSONLong): LocalDate = LocalDate.ofEpochDay(bson.value)
  }

  implicit object PlotIdHandler extends BSONHandler[BSONDocument, PlotId] {

    override def write(id: PlotId): BSONDocument = BSONDocument("x" -> id.x, "z" -> id.z, "world" -> id.world)

    override def read(bson: BSONDocument): PlotId = PlotId(bson.getAs[Int]("x").get, bson.getAs[Int]("z").get, bson.getAs[String]("world").get)
  }

  implicit object PlotWorldWriter extends BSONWriter[PlotWorld, BSONString] {

    override def write(w: PlotWorld): BSONString = BSONString(w.name)

  }

  implicit object PlayerDataHandler extends BSONDocumentWriter[PlayerData] with BSONDocumentReader[PlayerData] {
    override def write(self: PlayerData): BSONDocument = {
      import self._
      BSONDocument(
        "_id" -> id,
        "name" -> name,
        "lowerName" -> name.toLowerCase,
        "usernames" -> usernames,
        "displayName" -> displayName,
        "group" -> group,
        "perks" -> perks,
        "tier" -> tier,
        "plotLimit" -> plotLimit,
        "voteCredits" -> voteCredits
      ) ++ BSONDocument(
        "firstLogin" -> playTime.firstLogin,
        "lastSeen" -> playTime.lastSeen,
        "playTime" -> playTime.amount
      )
    }

    override def read(doc: BSONDocument): PlayerData = {
      val _id = doc.getAs[UUID]("_id").get
      val _name = doc.getAs[String]("name").get
      val _usernames = doc.getAs[mutable.LinkedHashSet[String]]("usernames").getOrElse(new mutable.LinkedHashSet)
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

  implicit object PlotHandler extends BSONDocumentWriter[Plot] with BSONDocumentReader[Plot] {
    override def write(self: Plot): BSONDocument = {
      BSONDocument(
        "_id" -> self.id,
        "owner" -> self.owner,
        "alias" -> self.alias,
        "timeClaimed" -> self.timeClaimed,
        "lastCleared" -> self.lastCleared,
        "expirationDate" -> self.expirationDate,
        "protected" -> self.protect,
        "closed" -> self.closed,
        "roadAccess" -> self.roadAccess
      ) ++ BSONDocument(
        "helpers" -> self.helpers,
        "trusted" -> self.trusted,
        "banned" -> self.banned
      )
    }

    override def read(doc: BSONDocument): Plot = {
      val _id = doc.getAs[PlotId]("_id").get
      val _owner = doc.getAs[UUID]("owner").get
      val _alias = doc.getAs[String]("alias")
      val _timeClaimed = doc.getAs[Instant]("timeClaimed").get
      val _lastCleared = doc.getAs[Instant]("lastCleared")
      val _expirationDate = doc.getAs[LocalDate]("expirationDate").get
      val _protected = doc.getAs[Boolean]("protected").get
      val _closed = doc.getAs[Boolean]("closed").get
      val _roadAccess = doc.getAs[Boolean]("roadAccess").get
      val _helpers = doc.getAs[Set[UUID]]("helpers").getOrElse(Set())
      val _trusted = doc.getAs[Set[UUID]]("trusted").getOrElse(Set())
      val _banned = doc.getAs[Set[UUID]]("banned").getOrElse(Set())
      Plot(
        id = _id,
        owner = _owner,
        alias = _alias,
        timeClaimed = _timeClaimed,
        lastCleared = _lastCleared,
        expirationDate = _expirationDate,
        protect = _protected,
        closed = _closed,
        roadAccess = _roadAccess,
        helpers = _helpers,
        trusted = _trusted,
        banned = _banned
      )
    }
  }

}

trait MongoRepository {

  def ensureIndex(col: BSONCollection, name: String, index: Index): Unit = {
    logger.info(s"Ensuring index '$name'...")
    col.indexesManager.ensure(index) onComplete {
      case Success(created) =>
        if (created) logger.info(s"Created '$name' index")
        else logger.info("Index already existed")
      case Failure(t) =>
        logger.log(Level.SEVERE, s"Failed to create index '$name'", t)
    }
  }

  def ensureIndexes(): Unit

  def logger: Logger

}

class MongoPlayerRepository(mongo: DB, val logger: Logger) extends MongoRepository
  with PlayerRepository with BSONHandlers {

  val col = mongo.collection[BSONCollection]("players")

  def queryById(id: UUID): BSONDocument = BSONDocument("_id" -> id)

  def queryByName(name: String): BSONDocument = BSONDocument("lowerName" -> name.toLowerCase)

  override def ensureIndexes(): Unit = {
    ensureIndex(col, "Lowercase Username", Index(key = List(("lowerName", IndexType.Ascending))))
  }

  override def insert(data: PlayerData): Unit = col.insert(data)

  override def save(data: PlayerData): Unit = col.save(data)

  override def search(id: UUID): Future[Option[PlayerData]] =
    col.find(queryById(id)).cursor[PlayerData].headOption

  override def searchAll(name: String): Future[Seq[PlayerData]] =
    col.find(queryByName(name)).cursor[PlayerData].collect[Vector]()

  override def delete(id: UUID): Unit = col.remove(queryById(id))

}

class MongoPlotRepository(mongo: DB, val logger: Logger) extends MongoRepository
  with PlotRepository with BSONHandlers {

  val col = mongo.collection[BSONCollection]("plots")

  override def ensureIndexes(): Unit = {}

  def queryById(w: PlotWorld, id: PlotId): BSONDocument = BSONDocument("_id" -> id)

  def queryByWorld(w: PlotWorld): BSONDocument = BSONDocument("_id.world" -> w.name)

  override def findAll(w: PlotWorld): Future[Seq[Plot]] =
    col.find(queryByWorld(w)).cursor[Plot].collect[Vector]()

  override def find(w: PlotWorld, id: PlotId): Future[Option[Plot]] =
    col.find(queryById(w, id)).cursor[Plot].headOption

  override def insert(w: PlotWorld, p: Plot): Unit = col.insert(p)

  override def delete(w: PlotWorld, id: PlotId): Unit = col.remove(queryById(w, id))

  override def save(w: PlotWorld, p: Plot): Unit = col.save(p)
}