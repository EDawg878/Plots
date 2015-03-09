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
import scala.util.Success

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object ApplicationLive {

  case class PlayerData(id: UUID,
                        name: String,
                        displayName: String,
                        tier: Int,
                        plotLimit: Int,
                        voteCredits: Int)

  trait Repository[K, V] {

    def find(id: K): Future[V]

    def save(data: V): Unit

  }

  trait PlayerRepository {

    def find(id: UUID): Future[Option[PlayerData]]

    def search(name: String): Future[Seq[PlayerData]]

    def findAll: Future[Iterable[PlayerData]]

    def save(data: PlayerData): Unit

    def delete(data: PlayerData): Unit

  }

  trait BSONHandlers {
    implicit object UUIDHandler extends BSONHandler[BSONBinary, UUID] {

      private def toBytes(id: UUID): Array[Byte] = {
        val shifts = Array.range(0, 64, 8).reverse
        def split(bits: Long) =
          shifts map(bits >>> _ ) map(_.asInstanceOf[Byte])
        split(id.getMostSignificantBits) ++ split(id.getLeastSignificantBits)
      }

      private def fromBytes(data: Array[Byte]): UUID = {
        @tailrec
        def read(r: Range, acc: Long = 0): Long = {
          if (r.head == r.last) acc
          else read(r.drop(1), (acc << 8) | (data(r.head) & 0xff))
        }
        new UUID(read(0 to 8), read(8 to 16))
      }

      override def write(id: UUID): BSONBinary = BSONBinary(toBytes(id), UuidSubtype)

      override def read(bson: BSONBinary): UUID = fromBytes(bson.value.readArray(bson.value.size))
    }
  }

  class MongoPlayerRepository extends PlayerRepository with BSONHandlers {

    val driver = new MongoDriver
    val conn = driver.connection(List("localhost"))
    val db = conn.db("minecraft")
    val col = db.collection[BSONCollection]("test")

    val index = Index(key = List(("lowerName", IndexType.Descending)))
    col.indexesManager.ensure(index) onComplete {
      case Success(v) =>
      case _ =>
    }

    private def fromBSON(doc: BSONDocument): PlayerData = {
      val id = doc.getAs[UUID]("_id").get
      val name = doc.getAs[String]("name").get
      val displayName = doc.getAs[String]("displayName").get
      val tier = doc.getAs[Int]("tier").get
      val plotLimit = doc.getAs[Int]("plotLimit").get
      val voteCredits = doc.getAs[Int]("voteCredits").get
      PlayerData(id, name, displayName, tier, voteCredits, plotLimit)
    }

    private def toBSON(data: PlayerData): BSONDocument = {
      BSONDocument(
      "_id" -> data.id,
      "name" -> data.name,
      "lowerName" -> data.name.toLowerCase,
      "displayName" -> data.displayName,
      "tier" -> data.tier,
      "plotLimit" -> data.plotLimit,
      "voteCredits" -> data.voteCredits
      )
    }

    override def find(id: UUID): Future[Option[PlayerData]] = {
      val query = BSONDocument("_id" -> id)
      col.find(query).cursor[BSONDocument].headOption.map(_.map(fromBSON))
    }

    override def save(data: PlayerData): Unit = ???

    override def search(name: String): Future[Seq[PlayerData]] = {
      val query = BSONDocument("lowerName" -> name.toLowerCase)
      col.find(query).cursor[BSONDocument].collect[List]().map(_.map(fromBSON))
    }

    override def findAll: Future[Iterable[PlayerData]] = ???

    override def delete(data: PlayerData): Unit = ???
  }

  def b(x: PlayerRepository) {
    x.find(UUID.randomUUID())
  }


}
