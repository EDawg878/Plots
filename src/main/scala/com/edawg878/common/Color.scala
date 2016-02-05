package com.edawg878.common

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Color {

  import org.bukkit.ChatColor._

  val Error = RED
  val Primary = GOLD
  val Secondary = RED

  implicit class Formatter(sc: StringContext) {

    def info(args: Any*): String =
      Primary + sc.standardInterpolator(identity, args.map(x => Secondary.toString + x + Primary))

    def err(args: Any*): String =
      Error + sc.standardInterpolator(identity, args)

  }
}