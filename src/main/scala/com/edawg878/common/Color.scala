package com.edawg878.common

import net.md_5.bungee.api.ChatColor

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Color {

  val Error = ChatColor.RED.toString
  val Primary = ChatColor.GOLD.toString
  val Secondary = ChatColor.RED.toString

  implicit class Formatter(sc: StringContext) {

    def info(args: Any*): String =
      sc.standardInterpolator(identity, args.map(x => Color.Secondary + x + Color.Primary))

    def err(args: Any*): String =
      Color.Error + sc.standardInterpolator(identity, args)

  }
}