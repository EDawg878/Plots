package com.edawg878.bukkit.commands

import com.edawg878.bukkit.commands.Operation._
import com.edawg878.common.Conversions.{RichInt, RichPlayerData}
import com.edawg878.common.{PlayerData, PlayerRepository, Readers}
import com.softwaremill.quicklens.modify
import org.bukkit.command.CommandSender

import scala.concurrent.Future

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Tier {

  private case class Config(ops: Operation, data: Future[PlayerData], tier: Int)

  class TierCommand(val db: PlayerRepository) extends BukkitCommand[Config] with Readers with NumericalOperations {

    override val parser = new BukkitOptionParser[Config]("/tier") {
      arg[Operation]("<operation>") required() action { (x, c) =>
        c.copy(ops = x)
      } text "operations: +, -, set, show"
      arg[Future[PlayerData]]("<player>") required() action { (x, c) =>
        c.copy(data = x)
      } text "player to modify"
      arg[Int]("<amount>") optional() action { (x, c) =>
        c.copy(tier = x)
      } text "number of tiers to add/subtract/set"
    }

    override val default: Config = Config(ops = Operation.Info, data = null, tier = 1)

    override def handle(sender: CommandSender, c: Config): Unit =
      onComplete(sender, c.data) { data =>
        c.ops match {
          case Add | Subtract | Set =>
            val updated = modify(data)(_.counters.tier)
              .using(calculate(_, c.tier)(c.ops).clamp(0, 10))
            sender.sendMessage(updated.displayTier)
            db.save(updated)
          case Info => sender.sendMessage(data.displayTier)
        }
      }

  }

}