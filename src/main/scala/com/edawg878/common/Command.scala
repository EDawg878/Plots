package com.edawg878.common

import com.edawg878.common.Group.Group
import com.edawg878.common.MessageFormatter.Color
import scopt.ConsoleHandler.ConsoleHandler
import scopt.{Read, CustomOptionParser}

import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Command {

  trait ConfigCommand[C, S] extends ErrorConverter[S] {

    val default: C

    val parser: CustomOptionParser[C, S]

    def handle(sender: S, config: C): Unit

    def run(sender: S, args: Array[String]): Boolean =
      parser.parse(args, default)(sender).map(handle(sender, _)).isDefined

  }

  trait ErrorConverter[S] {

    def onComplete[A](sender: S, a: Future[A])(f: A => Unit): Unit = a.onComplete {
      case Success(v) => f(v)
      case Failure(t) => parser.reportError(t.getMessage)(sender)
    }

    def parser: CustomOptionParser[_, S]
  }

  object Bukkit {

    import org.bukkit.command.{Command, CommandExecutor, CommandSender}

    val handler: ConsoleHandler[CommandSender] = new ConsoleHandler[CommandSender] {

      override def print(sender: CommandSender, msg: String): Unit =
        sender.sendMessage(msg)

      override def exit(): Unit = {}

      override def printError(sender: CommandSender, msg: String): Unit =
        sender.sendMessage(Color.Error + msg)

    }

    class BukkitOptionParser[C](cmd: String) extends CustomOptionParser[C, CommandSender](cmd)(handler)

    abstract class BukkitCommand[C] extends ConfigCommand[C, CommandSender] with CommandExecutor {

      override def onCommand(sender: CommandSender, command: Command, label: String, args: Array[String]): Boolean = {
        run(sender, args)
      }

    }
  }

  object Bungee {

    import net.md_5.bungee.api.CommandSender
    import net.md_5.bungee.api.chat.TextComponent
    import net.md_5.bungee.api.plugin.Command

    val handler: ConsoleHandler[CommandSender] = new ConsoleHandler[CommandSender] {

      override def print(sender: CommandSender, msg: String): Unit =
        sender.sendMessage(TextComponent.fromLegacyText(msg): _*)

      override def exit(): Unit = {}

      override def printError(sender: CommandSender, msg: String): Unit =
        sender.sendMessage(TextComponent.fromLegacyText(Color.Error + msg): _*)
    }

    abstract class BungeeCommand[C](cmd: String) extends Command(cmd) with ConfigCommand[C, CommandSender] {

      override def execute(sender: CommandSender, args: Array[String]): Unit = run(sender, args)

    }

  }

  class IllegalOperation extends IllegalArgumentException("Invalid Operation")

  object IntOps {

    type IntOp = (Int, Int) => Int

    val Add: IntOp = (x, y) => x + y

    val Subtract: IntOp = (x, y) => x - y

    val Set: IntOp = (_, y) => y

    val Show: IntOp = (x, _) => x

    implicit val reader: Read[IntOp] = Read.reads {
      case "+" => Add
      case "-" => Subtract
      case "set" => Set
      case "show" => Show
      case _ => throw new IllegalOperation
    }

  }

  object PerkOps {

    type PerkOp = (Set[String], String) => Set[String]

    val Add: PerkOp = (col, str) => col + str

    val Subtract: PerkOp = (col, str) => col - str

    val Show: PerkOp = (col, _) => col

    val Clear: PerkOp = (_, _) => Set.empty

    implicit val reader: Read[PerkOp] = Read.reads {
      case "+" => Add
      case "-" => Subtract
      case "show" => Show
      case "clear" => Clear
      case _ => throw new IllegalOperation
    }

  }

  object GroupOps {

    type GroupOp = (Group, Group) => Group

    val Promote: GroupOp = (a, _) => a.promote

    val Demote: GroupOp = (a, _) => a.demote

    val Set: GroupOp = (_, b) => b

    val Show: GroupOp = (a, _) => a

    implicit val reader: Read[GroupOp] = Read.reads {
      case "promote" => Promote
      case "demote" => Demote
      case "set" => Set
      case "show" => Show
      case _ => throw new IllegalOperation
    }

  }

}
