package com.edawg878.bukkit.commands

import com.edawg878.common.Command.{CommandMeta, BasicConfig}
import com.edawg878.common.Command.Bukkit._
import com.edawg878.common.Server.Server
import com.edawg878.common.PlayerRepository
import org.bukkit.command.CommandSender

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object PlayTime {

  class PlayTimeCommand(val db: PlayerRepository, server: Server) extends BasicBukkitCommand {

    override def meta = CommandMeta(cmd = "playtime", perm = None)

    override def handle(sender: CommandSender, c: BasicConfig): Unit = onComplete(sender, c.data) { data =>
      val online = server.isOnline(data.id)
      sender.sendMessage(data.displayPlayTime(online))
    }

  }

}
