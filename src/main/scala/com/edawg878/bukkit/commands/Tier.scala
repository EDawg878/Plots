package com.edawg878.bukkit.commands

import com.edawg878.common.Command.{IntOp, IntOps}
import com.edawg878.common.Readers.PlayerDataReader
import com.edawg878.common.{PlayerData, PlayerRepository}
import com.edawg878.common.Conversions.RichInt
import org.bukkit.command.CommandSender

import scala.concurrent.Future

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Tier {

  private[Tier] case class Config(fn: IntOp, data: Future[PlayerData], tier: Int)

  class TierCommand(val db: PlayerRepository) extends BukkitCommand[Config] with PlayerDataReader with IntOps {

    override val parser = new BukkitOptionParser[Config]("/tier") {
      arg[IntOp]("<operation>") required() action { (x, c) =>
        c.copy(fn = x)
      } text "operations: +, -, set, show"
      arg[Future[PlayerData]]("<player>") required() action { (x, c) =>
        c.copy(data = x)
      } text "player to modify"
      arg[Int]("<amount>") optional() action { (x, c) =>
        c.copy(tier = x)
      } text "number of tiers to add/subtract/set"
    }

    override val default: Config = Config(fn = Show, data = null, tier = 1)

    override def handle(sender: CommandSender, c: Config): Unit =
      onComplete(sender, c.data) { data =>
        c.fn match {
          case Add | Subtract | Set =>
            val updated = data.copy(tier = c.fn(data.tier, c.tier).clamp(0, 10))
            sender.sendMessage(updated.displayTier)
            db.save(updated)
          case Show => sender.sendMessage(data.displayTier)
        }
      }

  }

}