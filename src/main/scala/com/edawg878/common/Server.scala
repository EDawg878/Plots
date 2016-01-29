package com.edawg878.common

import java.io.{IOException, InputStream}
import java.nio.file.{Files, Path}
import java.util.UUID
import java.util.logging.Logger

import com.edawg878.bukkit.plot._
import org.bukkit._
import org.bukkit.block.Biome
import org.bukkit.entity.EntityType
import play.api.libs.json.{Reads, _}

import scala.concurrent.duration._
import scala.language.{higherKinds, implicitConversions}
import scala.util.Try

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Server {

  trait Plugin {

    def dataFolder: Path

    def resolveFile(name: String): Path = dataFolder.resolve(name)

    @throws(classOf[IOException])
    def saveResource(in: InputStream, out: String): Unit = Files.copy(in, resolveFile(out))

    def getResource(name: String): InputStream

    def logger: Logger

    def saveRootResource(path: Path): Unit = {
      Files.createDirectories(path.getParent)
      val name = path.getFileName.toString
      saveResource(getResource(name), name)
    }

  }

  case class Configuration[T : Reads](p: Plugin, path: Path) {

    def this(p: Plugin, s: String) = this(p, p.resolveFile(s))

    def bytes = Files.readAllBytes(path)

    def parse: T = Json.parse(bytes).as[T]

    def saveDefault(): Unit = {
      if (Files.notExists(path)) {
        Files.createDirectories(path.getParent)
        val name = path.getFileName.toString
        p.saveResource(p.getResource(name), name)
      }
    }

  }

  trait CustomReads {

    implicit val materialWrites = new Writes[Material] {
      override def writes(m: Material): JsValue = JsString(m.name.toLowerCase)
    }

    implicit val biomeReads = new Reads[Biome] {

      override def reads(json: JsValue): JsResult[Biome] = json match {
        case JsString(s) =>
          Try(Biome.valueOf(s.toUpperCase)).toOption.map(JsSuccess(_)).getOrElse(JsError("Biome value expected"))
        case _ => JsError("Biome value expected")
      }

    }

    implicit val biomeWrites = new Writes[Biome] {
      override def writes(b: Biome): JsValue = JsString(b.name.toLowerCase)
    }

    implicit val materialReads: Reads[Material] = __.read[String].map(Material.matchMaterial)

    implicit val plotStyleFormat = Json.format[PlotStyle]

    implicit val plotWorldConfigFormat =
      Json.format[PlotWorldConfig]

    implicit val worldEditConfigFormat =
      Json.format[WorldEditConfig]

    implicit val entityTypeFormat: Reads[EntityType] = __.read[String].map(EntityType.fromName)

    /*
  implicit val worldReads = new Reads[World] {

    override def reads(json: JsValue): JsResult[World] = json match {
      case JsString(s) =>
        Option(Bukkit.getWorld(s)).map(JsSuccess(_)).getOrElse(JsError("World value expected"))
      case _ => JsError("World value expected")
    }

  }

  implicit val worldWrites = new Writes[World] {
    override def writes(w: World): JsValue = JsString(w.getName)
  }

  implicit val locationReads: Reads[Location] = (
    (__ \ "world").read[World] ~
      (__ \ "x").read[Double] ~
      (__ \ "y").read[Double] ~
      (__ \ "z").read[Double] ~
      (__ \ "yaw").read[Float] ~
      (__ \ "pitch").read[Float]
    )((world, x, y, z, yaw, pitch) => new Location(world, x, y, z, yaw, pitch))

  implicit val locationWrites = new Writes[Location] {
    override def writes(l: Location) = Json.obj(
      "world" -> l.getWorld.getName,
      "x" -> l.getX,
      "y" -> l.getY,
      "z" -> l.getZ,
      "yaw" -> l.getYaw,
      "pitch" -> l.getPitch
    )
  }*/

  }

  trait Console {

    def sendMessage(message: String)

    def sendMessage(messages: String*): Unit = messages.foreach(sendMessage)

    def hasPermission(permission: String): Boolean

  }

  trait Player {

    def sendMessage(message: String)

    def sendMessage(messages: String*): Unit = messages.foreach(sendMessage)

    def hasPermission(permission: String): Boolean

    def getDisplayName: String

    def name: String

    def id: UUID

  }

  implicit class RichDuration(d: Duration) {

    def toTicks: Long = d.toSeconds * 20

  }

  trait Schedulable extends Runnable {

    def period: Duration

    def delay: Long = 0L

    def async: Boolean = false

  }

  trait Task {

    def cancel(): Unit

  }

  trait Server {

    def getPlayer(name: String): Option[Player]

    def getPlayer(id: UUID): Option[Player]

    def isOnline(name: String): Boolean = getPlayer(name).isDefined

    def isOnline(id: UUID): Boolean = getPlayer(id).isDefined

    def sync(f: Runnable, delay: Long = 0L): Unit

    def async(f: => Unit, delay: Long = 0L): Unit

    def schedule(period: Duration, delay: Long = 0L, f: => Unit): Task

    def scheduleAsync(period: Duration, delay: Long = 0L, f: => Unit): Task

    def shutdown(): Unit

  }

  trait Reloadable {

    def reload(): Unit

  }

}
