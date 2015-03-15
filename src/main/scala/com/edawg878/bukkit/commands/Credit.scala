package com.edawg878.bukkit.commands

import com.edawg878.bukkit.commands.Operation._
import com.edawg878.common.Conversions.RichPlayerData
import com.edawg878.common.{PlayerData, PlayerRepository, Readers}
import com.softwaremill.quicklens.modify
import org.bukkit.command.CommandSender

import scala.concurrent.Future

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Credit {

  private case class CreditConfig(ops: Operation, data: Future[PlayerData], credits: Int)

  class CreditCommand(val db: PlayerRepository) extends BukkitCommand[CreditConfig] with Readers with NumericalOperations {

    override val parser = new BukkitOptionParser[CreditConfig]("/credit") {
      arg[Operation]("<operation>") required() action { (x, c) =>
        c.copy(ops = x)
      } text "operations: +, -, set, show"
      arg[Future[PlayerData]]("<player>") required() action { (x, c) =>
        c.copy(data = x)
      } text "player to modify"
      arg[Int]("<amount>") optional() action { (x, c) =>
        c.copy(credits = x)
      }
    }

    override val default: CreditConfig = CreditConfig(ops = Operation.Info, data = null, credits = 1)

    override def handle(sender: CommandSender, c: CreditConfig): Unit =
      onComplete(sender, c.data) { data =>
        c.ops match {
          case Add | Subtract | Set =>
            val updated = modify(data)(_.counters.voteCredits)
              .using(calculate(_, c.credits)(c.ops).max(0))
            sender.sendMessage(updated.displayCredits)
            db.save(updated)
          case Info => sender.sendMessage(data.displayCredits)
        }
      }

  }

}