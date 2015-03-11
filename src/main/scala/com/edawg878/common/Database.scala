package com.edawg878.common

import java.util.UUID

import com.edawg878.common.Logging.PluginLogging
import reactivemongo.api.MongoDriver
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.api.indexes.{Index, IndexType}
import reactivemongo.bson.Subtype.UuidSubtype
import reactivemongo.bson._

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Database {

  case class PlayerData(id: UUID,
                        name: String,
                        usernames: Set[String],
                        displayName: Option[String],
                        counters: Counters) {

    def this(p: Player) =
      this(p.getUniqueId, p.getName, Set.empty, None, Counters(tier = 0, plotLimit = 1, voteCredits = 0))

  }

  case class Counters(tier: Int, plotLimit: Int, voteCredits: Int)

  trait PlayerRepository extends PluginLogging {

    def find(p: Player): Future[PlayerData] = {
      find(p.getUniqueId) map {
        case Some(data) =>
          logger.info(s"some $data")
          data
        case None =>
          val res = insert(p)
          logger.info(s"none $res")
          res
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

  class MongoPlayerRepository extends PlayerRepository with BSONHandlers with PluginLogging {

    val driver = new MongoDriver
    val conn = driver.connection(List("localhost"))
    val db = conn.db("minecraft")
    val col = db.collection[BSONCollection]("players")

    val index = Index(key = List(("lowerName", IndexType.Descending)))
    logger.info(s"Ensuring index `lowerName`...")
    col.indexesManager.ensure(index) onComplete {
      case Success(created) =>
        if (created) logger.info(s"Created `lowerName` index")
        else logger.info("Index already existed")
      case Failure(t) =>
        logger.severe("Failed to create index `lowerName`")
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

}