package com.edawg878.common

import java.io.{InputStream, IOException}
import java.nio.file.{Files, Path}
import java.util.UUID
import java.util.logging.Logger
import com.edawg878.bukkit.plot._
import org.bukkit.block.Biome
import play.api.data.validation.ValidationError
import play.api.libs.json._
import reactivemongo.bson._

import scala.collection.JavaConverters._

import org.bukkit._

import scala.collection.generic.CanBuildFrom
import scala.language.{implicitConversions, higherKinds}
import scala.util.Try

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Server {

  trait Plugin {

    def dataFolder: Path

    def resolveFile(name: String) = dataFolder.resolve(name)

    @throws(classOf[IOException])
    def saveResource(in: InputStream, out: String): Unit = Files.copy(in, resolveFile(out))

    def getResource(name: String): InputStream

    def logger: Logger

  }

  /*
  sealed trait YamlValue

  case class YamlString(value: String) extends YamlValue
  case class YamlArray(stream: Stream[Try[YamlValue]]) extends YamlValue {

  }

  implicit object StringHandler extends YamlHandler[YamlString, String] {
    override def write(s: String): YamlString = YamlString(s)

    override def read(yaml: YamlString): String = yaml.value
  }

  implicit object IntHandler extends YamlHandler[YamlString, Int] {
    override def write(int: Int): YamlString = YamlString(int.toString)

    override def read(yaml: YamlString): Int = yaml.value.toInt
  }

  implicit object DoubleHandler extends YamlHandler[YamlString, Double] {
    override def write(double: Double): YamlString = YamlString(double.toString)

    override def read(yaml: YamlString): Double = yaml.value.toDouble
  }

  implicit object FloatHandler extends YamlHandler[YamlString, Float] {
    override def write(float: Float): YamlString = YamlString(float.toString)

    override def read(yaml: YamlString): Float = yaml.value.toFloat
  }

  implicit object LongHandler extends YamlHandler[YamlString, Long] {
    override def write(long: Long): YamlString = YamlString(long.toString)

    override def read(yaml: YamlString): Long = yaml.value.toLong
  }

  implicit object LocationHandler extends YamlHandler[YamlString, Location] {

    val separator = "|"

    override def write(loc: Location): YamlString =
      YamlString(Seq(loc.getWorld.getName, loc.getX, loc.getY, loc.getZ, loc.getYaw, loc.getPitch).mkString(separator))

    override def read(yaml: YamlString): Location = {
      val s = yaml.value.split(separator)
      new Location(Bukkit.getWorld(s(0)), s(1).toDouble, s(2).toDouble, s(3).toDouble, s(4).toFloat, s(5).toFloat)
    }

  }



  object Yaml {

    def readObj(o: AnyRef): Option[YamlValue] = o match {
      case str: String => Some(YamlString(str))
      case strList: List[_] @unchecked => ??? //Some(YamlArray(strList.toStream))
      case _ => None
    }

  }

  trait YamlReader[Y <: YamlValue, T] {

    def read(yaml: Y): T
    def readOpt(yaml: Y): Option[T] = readTry(yaml).toOption
    def readTry(yaml: Y): Try[T] = Try(read(yaml))

  }

  trait YamlWriter[T, Y <: YamlValue] {

    def write(t: T): Y
    def writeOpt(t: T): Option[Y] = writeTry(t).toOption
    def writeTry(t: T): Try[Y] = Try(write(t))

  }

  trait VariantYamlReader[-Y <: YamlValue, +T] {

    def read(yaml: Y): T
    def readOpt(yaml: Y): Option[T] = readTry(yaml).toOption
    def readTry(yaml: Y): Try[T] = Try(read(yaml))

  }

  trait VariantYamlWriter[-T, +Y <: BSONValue] {

    def write(t: T): Y
    def writeOpt(t: T): Option[Y] = writeTry(t).toOption
    def writeTry(t: T): Try[Y] = Try(write(t))
  }


  trait YamlHandler[Y <: YamlValue, T] extends YamlReader[Y, T] with YamlWriter[T, Y]

  trait ConfigurationSection {

    def getAs[T](path: String)(implicit reader: YamlReader[_ <: YamlValue, T]): Option[T] =
      get(path).flatMap(reader.asInstanceOf[YamlReader[YamlValue, T]].readOpt)

    def set[T](path: String, t: T)(implicit writer: YamlWriter[T, _ <: YamlValue]): Unit =
      writer.asInstanceOf[YamlWriter[T, YamlValue]].writeOpt(t).foreach(set(path, _))

    def get(path: String): Option[YamlValue]

    def set(path: String, yaml: YamlValue): Unit

    def getSection(path: String): Option[ConfigurationSection]

    def getKeys: Iterable[String]

  }

  abstract class Configuration(path: Path) extends ConfigurationSection {

    def reload: Configuration

    def save(): Unit

    def saveDefault(plugin: Plugin): Unit = {
      if (Files.notExists(path)) {
        Files.createDirectories(path.getParent)
        val name = path.getFileName.toString
        plugin.saveResource(plugin.getResource(name), name)
      }
    }

    def getSection(path: String): Option[ConfigurationSection]

  }
  */


  trait CustomCombinators {

    import play.api.libs.functional.syntax._


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

    implicit val plotWorldConfigFormat = Json.format[PlotWorldConfig]

  }


  object Configuration extends CustomCombinators {

    def load(p: Plugin, f: Path): Seq[PlotWorldConfig] = {
      saveDefault(p, f)
      Json.parse(Files.readAllBytes(f)).as[Seq[PlotWorldConfig]]
    }

    def saveDefault(p: Plugin, f: Path): Unit = {
      if (Files.notExists(f)) {
        Files.createDirectories(f.getParent)
        val name = f.getFileName.toString
        p.saveResource(p.getResource(name), name)
      }
    }

  }

  trait AConfiguration {

    def save(): Unit

    def saveDefault(plugin: Plugin): Unit = {
      if (Files.notExists(file)) {
        Files.createDirectories(file.getParent)
        val name = file.getFileName.toString
        plugin.saveResource(plugin.getResource(name), name)
      }
    }

    def file: Path

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

  trait Server {

    def getPlayer(name: String): Option[Player]

    def getPlayer(id: UUID): Option[Player]

    def isOnline(name: String): Boolean = getPlayer(name).isDefined

    def isOnline(id: UUID): Boolean = getPlayer(id).isDefined

    def sync(f: => Unit): Unit

  }

  trait Reloadable {

    def reload(): Unit

  }

}
