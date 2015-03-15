package com.edawg878.bukkit.commands

import com.edawg878.common.Conversions.RichPlayerData
import com.edawg878.common.{PlayerData, PlayerRepository, Readers}
import com.softwaremill.quicklens._
import org.bukkit.command.CommandSender
import scopt.Read

import scala.concurrent.Future

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Perk {

  private object Operation extends Enumeration {
    type Operation = Value
    val Add, Subtract, Info, Clear = Value
  }

  import com.edawg878.bukkit.commands.Perk.Operation._

  private[Perk] case class Config(ops: Operation, data: Future[PlayerData], perk: String)

  class PerkCommand(val db: PlayerRepository) extends BukkitCommand[Config] with Readers {

    implicit val operationReader: Read[Operation] =
      Read.reads {
        case "+" => Operation.Add
        case "-" => Operation.Subtract
        case "show" => Operation.Info
        case "clear" => Operation.Clear
        case _ => throw new IllegalArgumentException("Invalid Operation")
      }

    def calculate(perks: Set[String], c: Config): Set[String] = c.ops match {
      case Add => perks + c.perk
      case Subtract => perks - c.perk
      case Clear => Set.empty
      case _ => perks
    }

    override val default = Config(ops = Operation.Info, data = null, perk = "")

    override val parser = new BukkitOptionParser[Config]("/perk") {
      arg[Operation]("<operation>") required() action { (x, c) =>
        c.copy(ops = x)
      } text "operations: +, -, clear, show"
      arg[Future[PlayerData]]("<player>") required() action { (x, c) =>
        c.copy(data = x)
      } text "player to modify"
      arg[String]("<perk>") optional() action { (x, c) =>
        c.copy(perk = x)
      } text "perk to add/subtract"
      checkConfig(c => c.ops match {
        case Add | Subtract =>
          if (c.perk.trim.isEmpty) failure("You must specify a perk")
          else success
        case _ => success
      })
    }

    override def handle(sender: CommandSender, c: Config): Unit =
      onComplete(sender, c.data) { data =>
        c.ops match {
          case Add | Subtract | Clear =>
            val updated = modify(data)(_.perks)
              .using(calculate(_, c))
            sender.sendMessage(updated.displayPerks)
            db.save(updated)
          case Info => sender.sendMessage(data.displayPerks)
        }
      }
  }

}