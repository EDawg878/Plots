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
      Primary + sc.standardInterpolator(identity, args.map(x => Secondary.toString + x + Primary))

    def err(args: Any*): String =
      Error + sc.standardInterpolator(identity, args)

  }
}