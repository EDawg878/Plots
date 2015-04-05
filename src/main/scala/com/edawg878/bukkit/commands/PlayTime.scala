package com.edawg878.bukkit.commands

import com.edawg878.common.Command.Bukkit.{BukkitOptionParser, BukkitCommand}
import com.edawg878.common.Readers.PlayerDataReader
import com.edawg878.common.Server.Server
import com.edawg878.common.{PlayerRepository, PlayerData}
import org.bukkit.command.CommandSender

import scala.concurrent.Future

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object PlayTime {

  case class Config(data: Future[PlayerData])

  class PlayTimeCommand(val db: PlayerRepository, val server: Server) extends BukkitCommand[Config] with PlayerDataReader {

    override val default: Config = Config(data = null)

    override val parser = new BukkitOptionParser[Config]("/playtime") {
      // TODO: make this optional
      arg[Future[PlayerData]]("<player>") required() action { (x, c) =>
        c.copy(data = x)
      } text "player to modify"
    }

    override def handle(sender: CommandSender, c: Config): Unit =
      onComplete(sender, c.data) { data =>
        val online = server.isOnline(data.id)
        sender.sendMessage(data.displayPlayTime(online))
      }

  }

}
