package com.edawg878.common

import java.util.UUID
import java.util.logging.{Level, Logger}

import reactivemongo.api.MongoDriver
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.api.indexes.{Index, IndexType}
import reactivemongo.bson.Subtype.UuidSubtype
import reactivemongo.bson._


import com.edawg878.common.Conversions._

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

case class Counters(tier: Int = 0, plotLimit: Int = 1, voteCredits: Int = 0)

case class PlayerData(id: UUID,
                      name: String,
                      usernames: Set[String],
                      displayName: Option[String],
                      perks: Set[String],
                      counters: Counters) {

  def this(p: Player) =
    this(id = p.getUniqueId, name = p.getName, usernames = Set.empty, perks = Set.empty, displayName = None, counters = Counters())

}

trait PlayerRepository {

  def find(p: Player): Future[PlayerData] = {
    find(p.getUniqueId) map {
      case Some(data) => data
      case None => insert(p)
    }
  }

  def insert(p: Player): PlayerData = {
    val data = new PlayerData(p)
    insert(data)
    data
  }

  def find(id: UUID): Future[Option[PlayerData]]

  def insert(data: PlayerData): Unit

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

  implicit object Writer extends BSONDocumentWriter[PlayerData] {
    def write(self: PlayerData): BSONDocument = {
      val counters = self.counters
      BSONDocument(
        "_id" -> self.id,
        "name" -> self.name,
        "lowerName" -> self.name.toLowerCase,
        "usernames" -> self.usernames.toOption,
        "displayName" -> self.displayName,
        "perks" -> self.perks.toOption,
        "tier" -> counters.tier,
        "plotLimit" -> counters.plotLimit,
        "voteCredits" -> counters.voteCredits)
    }
  }

  implicit object Reader extends BSONDocumentReader[PlayerData] {
    def read(doc: BSONDocument): PlayerData = {
      val id = doc.getAs[UUID]("_id").get
      val name = doc.getAs[String]("name").get
      val usernames = doc.getAs[Set[String]]("usernames").getOrElse(Set.empty)
      val perks = doc.getAs[Set[String]]("perks").getOrElse(Set.empty)
      val displayName = doc.getAs[String]("displayName")
      val tier = doc.getAs[Int]("tier").get
      val plotLimit = doc.getAs[Int]("plotLimit").get
      val voteCredits = doc.getAs[Int]("voteCredits").get
      val counters = Counters(tier, plotLimit, voteCredits)
      PlayerData(id, name = name, usernames, displayName, perks, counters)
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

  def ensureIndexes(): Unit = {
    val index = Index(key = List(("lowerName", IndexType.Ascending)))
    logger.info("Ensuring index `lowerName`...")
    col.indexesManager.ensure(index) onComplete {
      case Success(created) =>
        if (created) logger.info("Created `lowerName` index")
        else logger.info("Index already existed")
      case Failure(t) =>
        logger.log(Level.SEVERE, "Failed to create index `lowerName`", t)
    }
  }

  override def find(id: UUID): Future[Option[PlayerData]] =
    col.find(queryById(id)).cursor[PlayerData]
      .headOption

  override def insert(data: PlayerData): Unit = col.insert(data)

  override def save(data: PlayerData): Unit = col.save(data)

  override def search(name: String): Future[Seq[PlayerData]] =
    col.find(queryByName(name)).cursor[PlayerData]
      .collect[Vector]()

  override def delete(id: UUID): Unit = col.remove(queryById(id))

}