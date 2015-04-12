package com.edawg878.common

import org.bukkit.Location
import com.edawg878.common.Server.CustomCombinators._
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsNumber, JsObject, Json}
import play.modules.reactivemongo.json.collection.JSONCollection
import reactivemongo.api.{MongoConnection, MongoDriver}
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future


/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
case class MyConfig(_id: String , locs: List[Location], loc: Location)

trait ConfigCombinators {
  implicit val myConfigFormat = Json.format[MyConfig]
}

trait ConfigRepository {

  def find(id: String): Future[Option[MyConfig]]

  def save(config: MyConfig): Unit

}

class MongoConfigRepository(drive: MongoDriver, conn: MongoConnection) extends ConfigRepository with ConfigCombinators {

  val db = conn.db("minecraft")
  val col = db.collection[JSONCollection]("config")

  override def find(id: String): Future[Option[MyConfig]] =
    col.find(Json.obj("_id" -> id)).cursor[JsObject].headOption.map(_.flatMap(Json.fromJson[MyConfig](_).asOpt))

  override def save(config: MyConfig): Unit = col.save(Json.toJson(config))
}