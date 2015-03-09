import java.util.UUID

import com.edawg878.common.PlayerData
import reactivemongo.api._
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Test extends App {

  override def main(args: Array[String]) = {
    val driver = new MongoDriver
    val connection = driver.connection(List("localhost"))
    val db = connection.db("minecraft")
    val collection = db.collection[BSONCollection]("test")
    val document = BSONDocument(
      "firstName" -> "Stephane",
      "lastName" -> "Godbillon",
      "age" -> 29)
    val future = collection.insert(document)
    future.onComplete {
      case Failure(e) => throw e
      case Success(lastError) =>
        println("successfully inserted document with lastError = " + lastError)
    }
  }

}