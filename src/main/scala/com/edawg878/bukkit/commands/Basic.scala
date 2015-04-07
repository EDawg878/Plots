package com.edawg878.bukkit.commands

import com.edawg878.common.Bukkit.BasicBukkitCommand
import com.edawg878.common.{BasicConfig, CommandMeta}
import com.edawg878.common.Server.Server
import com.edawg878.common.{DateUnit, PlayerRepository}
import net.md_5.bungee.api.ChatColor
import org.bukkit.command.CommandSender
import com.edawg878.common.DateUnit.Implicits.standardUnits
import com.edawg878.common.Color.Formatter

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Basic {

  class PlayTimeCommand(val db: PlayerRepository, server: Server) extends BasicBukkitCommand {

    override def meta = CommandMeta(cmd = "playtime", perm = None)

    override def handle(sender: CommandSender, c: BasicConfig): Unit = onComplete(sender, c.data) { data =>
      val online = server.isOnline(data.id)
      sender.sendMessage(data.displayPlayTime(online))
    }

  }

  class SeenCommand(val db: PlayerRepository, server: Server) extends BasicBukkitCommand {

    override def meta = CommandMeta(cmd = "seen", perm = None)

    override def handle(sender: CommandSender, c: BasicConfig): Unit = onComplete(sender, c.data) { data =>
      val online = if (server.isOnline(data.id)) ChatColor.GREEN + "online" else ChatColor.DARK_RED + "offline"
      val seen = DateUnit.format(data.playTime.activity).mkString(" ")
      sender.sendMessage(info"${data.name} has been $online for $seen")
    }

  }

}
