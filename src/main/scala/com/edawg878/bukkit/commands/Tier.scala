package com.edawg878.bukkit.commands

import com.edawg878.common.Command.Bukkit.{BukkitOptionParser, BukkitCommand}
import com.edawg878.common.Command.CommandMeta
import com.edawg878.common.Command.IntOps._
import com.edawg878.common.Readers.PlayerDataReader
import com.edawg878.common.{PlayerData, PlayerRepository}
import com.edawg878.common.Conversions.RichInt
import org.bukkit.command.CommandSender

import scala.concurrent.Future

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Tier {

  case class Config(op: IntOp, data: Future[PlayerData], tier: Int)

  class TierCommand(val db: PlayerRepository) extends BukkitCommand[Config] with PlayerDataReader {

    override def meta = CommandMeta(cmd = "tier", perm = None)

    override val default = Config(op = Show, data = null, tier = 1)

    override val parser = new BukkitOptionParser[Config]("/tier") {
      arg[IntOp]("<operation>") required() action { (x, c) =>
        c.copy(op = x)
      } text "operations: +, -, set, show"
      arg[Future[PlayerData]]("<player>") required() action { (x, c) =>
        c.copy(data = x)
      } text "player to modify"
      arg[Int]("<amount>") optional() action { (x, c) =>
        c.copy(tier = x)
      } text "number of tiers to add/subtract/set"
    }

    override def handle(sender: CommandSender, c: Config): Unit = onComplete(sender, c.data) { data =>
      c.op match {
        case Add | Subtract | Set =>
          val updated = data.copy(tier = c.op.using(data.tier, c.tier).clamp(0, 10))
          sender.sendMessage(updated.displayTier)
          db.save(updated)
        case Show => sender.sendMessage(data.displayTier)
      }
    }

  }

}