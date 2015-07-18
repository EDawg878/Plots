package com.edawg878.common

import net.md_5.bungee.api.ChatColor

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Color {

  val Error = ChatColor.RED
  val Primary = ChatColor.GOLD
  val Secondary = ChatColor.RED

  implicit class Formatter(sc: StringContext) {

    def info(args: Any*): String =
      Color.Primary + sc.standardInterpolator(identity, args.map(x => Color.Secondary.toString + x + Color.Primary))

    def err(args: Any*): String =
      Color.Error + sc.standardInterpolator(identity, args)

  }
}