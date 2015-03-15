package com.edawg878.bukkit.commands

import com.edawg878.common.Conversions._
import com.edawg878.common._
import com.softwaremill.quicklens._
import org.bukkit.Bukkit
import org.bukkit.command.{Command, CommandExecutor, CommandSender}
import org.bukkit.entity.Player
import scopt.Read

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

class TestCommand extends CommandExecutor {

  case class Config(player: Player = null)

  implicit val player: Read[Player] =
    Read.reads(s =>
      Option(Bukkit.getPlayerExact(s)) match {
        case Some(p) => p
        case name =>
          throw new IllegalArgumentException(s"'$s' is not online")
      }
    )

  val parser = new BukkitOptionParser[Config]("/test") {
    arg[Player]("<player>") required() action { (x, c) =>
      c.copy(player = x)
    } text "username of player to smite"
  }

  override def onCommand(sender: CommandSender, command: Command, label: String, args: Array[String]): Boolean = {
    parser.parse(args, Config())(sender) match {
      case Some(config) =>
        config.player.sendMessage("You have been smited!")
        true
      case None => false
    }
  }

}

class TierCommand(val db: PlayerRepository) extends CommandExecutor with Readers {

  object Operation extends Enumeration {
    type Operation = Value
    val Add, Subtract, Set, Info = Value
  }

  import Operation._

  implicit val operationReader: Read[Operation] =
    Read.reads {
      case "plus"  => Operation.Add
      case "minus" => Operation.Subtract
      case "set"   => Operation.Set
      case _       => Operation.Info
    }

  case class Config(ops: Operation = null, data: Future[PlayerData] = null, tier: Int = 1)

  def calculate(x: Int, y: Int)(o: Operation): Int = o match {
    case Add      => x + y
    case Subtract => x - y
    case Set      => y
    case _        => x
  }

  val parser = new BukkitOptionParser[Config]("/tier") {
    arg[Operation]("<operation>") required() action { (x, c) =>
      c.copy(ops = x)
    } text "operations: plus, minus, set, info"
    arg[Future[PlayerData]]("<player>") required() action { (x, c) =>
      c.copy(data = x)
    } text "player to modify"
    arg[Int]("<amount>") optional() action { (x, c) =>
      c.copy(tier = x)
    } text "number of tiers"
  }

  override def onCommand(sender: CommandSender, command: Command, label: String, args: Array[String]): Boolean = {
    parser.parse(args, Config())(sender) match {
      case Some(config) =>
        config.data onComplete {
          case Success(data) =>
            config.ops match {
              case Add | Subtract | Set =>
                val updated = modify(data)(_.counters.tier)
                  .using(calculate(_, config.tier)(config.ops).clamp(0, 10))
                sender.sendMessage(updated.displayTier)
                db.save(updated)
              case Info => sender.sendMessage(data.displayTier)
            }
          case Failure(t) =>
            parser.reportError(t.getMessage)(sender)
        }
        true
      case _ => false
    }
  }

}

class PerkCommand(val db: PlayerRepository) extends CommandExecutor with Readers {

  object Operation extends Enumeration {
    type Operation = Value
    val Add, Subtract, Info, Clear = Value
  }

  import Operation._

  implicit val operationReader: Read[Operation] =
    Read.reads {
      case "plus" => Operation.Add
      case "minus" => Operation.Subtract
      case "info" => Operation.Info
      case "clear" => Operation.Clear
    }

  case class Config(ops: Operation = null, data: Future[PlayerData] = null, perk: String = "")

  def calculate(perks: Set[String], c: Config): Set[String] = c.ops match {
    case Add      => perks + c.perk
    case Subtract => perks - c.perk
    case Clear    => Set.empty
    case _        => perks
  }

  val parser = new BukkitOptionParser[Config]("/perk") {
    arg[Operation]("<operation>") required() action { (x, c) =>
      c.copy(ops = x)
    } text "operations: plus, minus, clear, info"
    arg[Future[PlayerData]]("<player>") required() action { (x, c) =>
      c.copy(data = x)
    } text "player to modify"
    arg[String]("<perk>") optional() action { (x, c) =>
      c.copy(perk = x)
    } text "perk to "
    checkConfig(c => c.ops match {
        case Add | Subtract =>
          if (c.perk.trim.isEmpty) failure("You must specify a perk")
          else success
        case _ => success
      }
    )
  }

  override def onCommand(sender: CommandSender, command: Command, label: String, args: Array[String]): Boolean = {
    parser.parse(args, Config())(sender) match {
      case Some(config) =>
        config.data onComplete {
          case Success(data) =>
            config.ops match {
              case Add | Subtract | Clear =>
                val updated = modify(data)(_.perks)
                  .using(calculate(_, config))
                sender.sendMessage(updated.displayPerks)
                db.save(updated)
              case Info => sender.sendMessage(data.displayPerks)
            }
          case Failure(t) =>
            parser.reportError(t.getMessage)(sender)
        }
        true
      case _ => false
    }
  }

}