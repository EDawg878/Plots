package com.edawg878.common

import java.text.MessageFormat

import net.md_5.bungee.api.ChatColor

import scala.collection.mutable

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object MessageFormatter {

  val cache = mutable.Map[String, MessageFormat]()

  object Color {
    val Error = ChatColor.RED.toString
    val Primary = ChatColor.GOLD.toString
    val Secondary = ChatColor.RED.toString
  }

  private def colorizeInfo(str: String): String = {
    val b = new mutable.StringBuilder(Color.Primary)

    str foreach {
      case '[' => b append Color.Secondary
      case ']' => b append Color.Primary
      case c => b append c
    }

    b.toString()
  }

  private def colorizeErr(str: String): String = Color.Error + str

  implicit class Formatter(val sc: StringContext) extends AnyVal {

    def fmt(args: Any*): MessageFormat = {
      val key = sc.parts.mkString
      cache.getOrElseUpdate(key, new MessageFormat(key))
    }

    def info(args: Any*): String =
      sc.standardInterpolator(colorizeInfo, args)

    def err(args: Any*): String =
      sc.standardInterpolator(colorizeErr, args)

  }

  implicit class RichMessageFormat(val self: MessageFormat) extends AnyVal {

    private def fmt(args: Seq[Any]): String =
      self.format(args.map(_.asInstanceOf[Object]).toArray)

    def info(args: Any*): String =
      colorizeInfo(fmt(args))

    def err(args: Any*): String =
      colorizeErr(fmt(args))

  }

}