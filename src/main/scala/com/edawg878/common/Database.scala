package com.edawg878.common

import java.util.UUID

import reactivemongo.api.MongoDriver
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.api.indexes.{IndexType, Index}
import reactivemongo.bson.Subtype.UuidSubtype
import reactivemongo.bson._

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
case class PlayerData(id: UUID,
                      name: String,
                      usernames: Set[String],
                      displayName: Option[String],
                      counters: Counters) {

  def this(p: Player) =
    this(p.getUniqueId, p.getName, Set, None, Counters)

}

case class Counters(tier: Int, plotLimit: Int, voteCredits: Int)

trait PlayerRepository {

  def find(p: Player): Future[PlayerData] = {
    find(p.getUniqueId).map {
      case Some(data) => data
      case None =>
        // log error
        insert(p)
    }
  }

  def insert(p: Player): PlayerData = {
    val data  = new PlayerData(p)
    insert(data)
    data
  }

  protected def find(id: UUID): Future[Option[PlayerData]]

  protected def insert(data: PlayerData): Unit

  def search(name: String): Future[Seq[PlayerData]]

  def save(data: PlayerData): Unit

  def delete(data: PlayerData): Unit

}

trait BSONHandlers {

  implicit object UUIDHandler extends BSONHandler[BSONBinary, UUID] {

    override def write(id: UUID): BSONBinary = BSONBinary(toBytes(id), UuidSubtype)

    private def toBytes(id: UUID): Array[Byte] = {
      val shifts = Array.range(0, 64, 8).reverse
      def split(bits: Long) =
        shifts map (bits >>> _) map (_.asInstanceOf[Byte])
      split(id.getMostSignificantBits) ++ split(id.getLeastSignificantBits)
    }

    override def read(bson: BSONBinary): UUID = fromBytes(bson.value.readArray(bson.value.size))

    private def fromBytes(data: Array[Byte]): UUID = {
      @tailrec
      def read(r: Range, acc: Long = 0): Long = {
        if (r.head == r.last) acc
        else read(r.drop(1), (acc << 8) | (data(r.head) & 0xff))
      }
      new UUID(read(0 to 8), read(8 to 16))
    }
  }

  implicit object Writer extends BSONDocumentWriter[PlayerData] {
    def write(self: PlayerData): BSONDocument = BSONDocument(
      "_id" -> self.id,
      "name" -> self.name,
      "usernames" -> self.usernames,
      "lowerName" -> self.name.toLowerCase,
      "displayName" -> self.displayName,
      "tier" -> self.counters.tier,
      "plotLimit" -> self.counters.plotLimit,
      "voteCredits" -> self.counters.plotLimit
    )
  }

  implicit object Reader extends BSONDocumentReader[PlayerData] {
    def read(doc: BSONDocument): PlayerData = {
      val id = doc.getAs[UUID]("_id").get
      val name = doc.getAs[String]("name").get
      val usernames = doc.getAs[Set[String]]("usernames").get
      val displayName = Some(doc.getAs[String]("displayName").get)
      val tier = doc.getAs[Int]("tier").get
      val plotLimit = doc.getAs[Int]("plotLimit").get
      val voteCredits = doc.getAs[Int]("voteCredits").get
      val counters = Counters(tier, plotLimit, voteCredits)
      PlayerData(id, name, usernames, displayName, counters)
    }
  }

}

class MongoPlayerRepository extends PlayerRepository with BSONHandlers {

  val driver = new MongoDriver
  val conn = driver.connection(List("localhost"))
  val db = conn.db("minecraft")
  val col = db.collection[BSONCollection]("test")

  val index = Index(key = List(("lowerName", IndexType.Descending)))
  // log ensuring index...
  col.indexesManager.ensure(index) onComplete  {
    case Success(b) => b match {
      case true =>
        // log creating index...
      case false =>
        // log index already existed
    }
    case Failure(t) =>
      // failed to create index
  }

  override def find(id: UUID): Future[Option[PlayerData]] = {
    val query = BSONDocument("_id" -> id)
    col.find(query).cursor[PlayerData].headOption
  }

  override def insert(data: PlayerData): Unit = col.insert(data)

  override def save(data: PlayerData): Unit = col.save(data)

  override def search(name: String): Future[Seq[PlayerData]] = {
    val query = BSONDocument("lowerName" -> name.toLowerCase)
    col.find(query).cursor[PlayerData].collect[Vector]()
  }

  override def delete(data: PlayerData): Unit = {
    val query = BSONDocument("_id" -> data.id)
    col.remove(query)
  }

}