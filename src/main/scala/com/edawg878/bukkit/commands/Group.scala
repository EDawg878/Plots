package com.edawg878.bukkit.commands

import com.edawg878.common.Command.Bukkit.{BukkitOptionParser, BukkitCommand}
import com.edawg878.common.Command.GroupOps._
import com.edawg878.common.Group._
import com.edawg878.common.Readers.PlayerDataReader
import com.edawg878.common.{PlayerRepository, PlayerData}
import org.bukkit.command.CommandSender
import scopt.CustomOptionParser
import com.edawg878.common.Readers.Implicits.groupReader

import scala.concurrent.Future

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Group {

  case class Config(fn: GroupOp, data: Future[PlayerData], group: Group)

  class GroupCommand(val db: PlayerRepository) extends BukkitCommand[Config] with PlayerDataReader {

    override val default: Config = Config(fn = Show, data = null, group = Default)

    override val parser: CustomOptionParser[Config, CommandSender] = new BukkitOptionParser[Config]("/group") {
      arg[GroupOp]("<operation>") required() action { (x,c) =>
        c.copy(fn = x)
      } text "operations: promote, demote, set, show"
      arg[Future[PlayerData]]("<player>") required() action { (x, c) =>
        c.copy(data = x)
      } text "player to modify"
      arg[Group]("<group>") optional() action { (x, c) =>
        c.copy(group = x)
      } text "group to set"
    }

    override def handle(sender: CommandSender, c: Config): Unit =
      onComplete(sender, c.data) { data =>
        c.fn match {
          case Promote | Demote =>
            val updated = data.copy(group = c.fn(data.group, c.group))
            sender.sendMessage(updated.displayGroup)
            db.save(updated)
          case Show => sender.sendMessage(data.displayGroup)
        }
      }

  }

}
