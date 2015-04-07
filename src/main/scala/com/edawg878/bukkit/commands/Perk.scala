package com.edawg878.bukkit.commands

import com.edawg878.common.Bukkit.{BukkitOptionParser, BukkitCommand}
import com.edawg878.common.{PerkOpsReader, CommandMeta, PlayerData, PlayerRepository}
import com.edawg878.common.PerkOps._
import com.edawg878.common.Operations.PerkOp
import com.edawg878.common.Readers.PlayerDataReader
import org.bukkit.command.CommandSender

import scala.concurrent.Future

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Perk {

  case class Config(fn: PerkOp, data: Future[PlayerData], perk: String)

  class PerkCommand(val db: PlayerRepository) extends BukkitCommand[Config] with PlayerDataReader {

    override def meta = CommandMeta(cmd = "perk", perm = None)

    override val default = Config(fn = Show, data = null, perk = "")

    override val parser = new BukkitOptionParser[Config]("/perk") with PerkOpsReader {
      arg[PerkOp]("<operation>") required() action { (x, c) =>
        c.copy(fn = x)
      } text "operations: +, -, clear, show"
      arg[Future[PlayerData]]("<player>") required() action { (x, c) =>
        c.copy(data = x)
      } text "player to modify"
      arg[String]("<perk>") optional() action { (x, c) =>
        c.copy(perk = x)
      } text "perk to add/subtract"
      checkConfig(c => c.fn match {
        case Add | Subtract =>
          if (c.perk.trim.isEmpty) failure("You must specify a perk")
          else success
        case _ => success
      })
    }

    override def handle(sender: CommandSender, c: Config): Unit = onComplete(sender, c.data) { data =>
      c.fn match {
        case Add | Subtract | Clear =>
          val updated = data.copy(perks = c.fn.using(data.perks, c.perk))
          sender.sendMessage(updated.displayPerks)
          db.save(updated)
        case Show => sender.sendMessage(data.displayPerks)
      }
    }

  }

}