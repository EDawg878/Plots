package com.edawg878.bukkit.commands

import com.edawg878.common.BukkitCommandHandler.{BukkitOptionParser, BukkitCommand}
import com.edawg878.common.IntOps._
import com.edawg878.common.Operations.IntOp
import com.edawg878.common.Readers.PlayerDataReader
import com.edawg878.common._
import org.bukkit.command.CommandSender

import scala.concurrent.Future

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Credit {

  case class Config(op: IntOp, data: Future[PlayerData], credits: Int)

  class CreditCommand(val db: PlayerRepository) extends BukkitCommand[Config]
    with PlayerDataReader with IntOpsReader {

    def meta = CommandMeta(cmd = "credit", perm = None)

    val default = Config(op = Show, data = null, credits = 1)

    val parser = new BukkitOptionParser[Config]("/credit") {
      arg[IntOp]("<operation>") required() action { (x, c) =>
        c.copy(op = x)
      } text "operations: +, -, set, show"
      arg[Future[PlayerData]]("<player>") required() action { (x, c) =>
        c.copy(data = x)
      } text "player to modify"
      arg[Int]("<amount>") optional() action { (x, c) =>
        c.copy(credits = x)
      } text "number of credits to add/subtract/set"
    }

    def handle(sender: CommandSender, c: Config): Unit = onComplete(sender, c.data) { data =>
      c.op match {
        case Add | Subtract | Set =>
          val updated = data.copy(voteCredits = c.op.using(data.voteCredits, c.credits).max(0))
          sender.sendMessage(updated.displayCredits)
          db.save(updated)
        case Show => sender.sendMessage(data.displayCredits)
      }
    }
  }

}