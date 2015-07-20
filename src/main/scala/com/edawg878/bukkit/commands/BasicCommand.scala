package com.edawg878.bukkit.commands

import com.edawg878.common.BukkitCommandHandler.BasicBukkitCommand
import com.edawg878.common._
import com.edawg878.common.Server.Server
import net.md_5.bungee.api.ChatColor
import org.bukkit.command.CommandSender
import com.edawg878.common.DateUnit.Implicits.standardUnits
import com.edawg878.common.Color.Formatter
import com.edawg878.common.Conversions._

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object BasicCommand {

  class PlayTimeCommand(val db: PlayerRepository, server: Server) extends BasicBukkitCommand {

    def meta = CommandMeta(cmd = "playtime", perm = None)

    def handle(sender: CommandSender, c: BasicConfig): Unit = onComplete(sender, c.data) { data =>
      val online = server.isOnline(data.id)
      sender.sendMessage(data.playTimeToString(online))
    }

  }

  class SeenCommand(val db: PlayerRepository, server: Server) extends BasicBukkitCommand {

    def meta = CommandMeta(cmd = "seen", perm = None)

    def handle(sender: CommandSender, c: BasicConfig): Unit = onComplete(sender, c.data) { data =>
      val online = if (server.isOnline(data.id)) ChatColor.GREEN + "online" else ChatColor.DARK_RED + "offline"
      val seen = DateUnit.format(data.playTime.activity).mkString(" ")
      sender.sendMessage(info"${data.name} has been $online for $seen")
    }

  }

  class WhoIsCommand(val db: PlayerRepository, server: Server) extends BasicBukkitCommand {

    def meta = CommandMeta(cmd = "whois", perm = None)

    def handle(sender: CommandSender, c: BasicConfig): Unit = onComplete(sender, c.data) { data =>
      sender.sendMessage(info"${data.name}'s uuid is ${data.id.toString}")
      if (data.usernames.isEmpty) sender.sendMessage(err"No previous usernames found")
      else {
        val history = data.usernames.toSeq.reverse.mkStringPretty
        sender.sendMessage(info"Previously known as: $history")
      }
    }

  }

}
