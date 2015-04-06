package com.edawg878.common

import com.edawg878.common.Group.Group
import com.edawg878.common.MessageFormatter.Color
import com.edawg878.common.Readers.PlayerDataReader
import org.bukkit.command.CommandSender
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

    def run(sender: S, args: Seq[String]): Boolean =
      parser.parse(args, default)(sender).map(handle(sender, _)).isDefined

  }

  trait ErrorConverter[S] {

    def onComplete[A](sender: S, a: Future[A])(f: A => Unit): Unit = a.onComplete {
      case Success(v) => f(v)
      case Failure(t) => parser.reportError(t.getMessage)(sender)
    }

    def parser: CustomOptionParser[_, S]
  }

  case class CommandMeta(cmd: String, perm: Option[String], aliases: String*)

  trait Command[S] {

    def meta: CommandMeta

    def execute(sender: S, args: Seq[String]): Unit

  }

  abstract class BaseCommand[C, S] extends Command[S] with ConfigCommand[C,S] {

    override def execute(sender: S, args: Seq[String]): Unit = run(sender, args)

  }

  case class BasicConfig(data: Future[PlayerData])

  abstract class BasicCommand[S](handler: ConsoleHandler[S]) extends BaseCommand[BasicConfig, S] with PlayerDataReader {

    val default: BasicConfig = BasicConfig(data = null)

    val parser = new CustomOptionParser[BasicConfig, S]("/" + meta.cmd)(handler) {
      arg[Future[PlayerData]]("<player>") required() action { (x, c) =>
        c.copy(data = x)
      } text "player to modify"
    }
  }

  object Bukkit {

    import org.bukkit.command.CommandSender

    val handler = new ConsoleHandler[CommandSender] {

      override def print(sender: CommandSender, msg: String): Unit =
        sender.sendMessage(msg)

      override def exit(): Unit = {}

      override def printError(sender: CommandSender, msg: String): Unit =
        sender.sendMessage(Color.Error + msg)
    }

    class BukkitOptionParser[C](cmd: String) extends CustomOptionParser[C, CommandSender](cmd)(handler)

    abstract class BukkitCommand[C] extends BaseCommand[C, CommandSender]

    abstract class BasicBukkitCommand extends BasicCommand[CommandSender](handler)

  }

  object Bungee {

    import net.md_5.bungee.api.CommandSender
    import net.md_5.bungee.api.chat.TextComponent

    val handler = new ConsoleHandler[CommandSender] {
      override def print(sender: CommandSender, msg: String): Unit =
        sender.sendMessage(TextComponent.fromLegacyText(msg): _*)

      override def exit(): Unit = {}

      override def printError(sender: CommandSender, msg: String): Unit =
        sender.sendMessage(TextComponent.fromLegacyText(Color.Error + msg): _*)
    }

    class BungeeOptionParser[C](cmd: String) extends CustomOptionParser[C, CommandSender](cmd)(handler)

    abstract class BungeeCommand[C] extends BaseCommand[C, CommandSender]

    abstract class BasicBukkitCommand extends BasicCommand[CommandSender](handler)

  }

  class IllegalOperation extends IllegalArgumentException("Invalid Operation")

  object IntOps {

    sealed trait IntOp {
      def using(x: Int, y: Int): Int
    }

    case object Add extends IntOp {
      def using(x: Int, y: Int) = x + y
    }
    case object Subtract extends IntOp {
      def using(x: Int, y: Int) = x - y
    }
    case object Set extends IntOp {
      def using(x: Int, y: Int) = y
    }
    case object Show extends IntOp {
      def using(x: Int, y: Int) = x
    }

    implicit val reader: Read[IntOp] = Read.reads {
      case "+" => Add
      case "-" => Subtract
      case "set" => Set
      case "show" => Show
      case _ => throw new IllegalOperation
    }

  }

  object PerkOps {

    sealed trait PerkOp {
      def using(col: Set[String], str: String): Set[String]
    }

    case object Add extends PerkOp {
      def using(col: Set[String], str: String) = col + str
    }
    case object Subtract extends PerkOp {
      def using(col: Set[String], str: String) = col - str
    }
    case object Show extends PerkOp {
      def using(col: Set[String], str: String) = col
    }
    case object Clear extends PerkOp {
      def using(col: Set[String], str: String) = Set.empty
    }

    implicit val reader: Read[PerkOp] = Read.reads {
      case "+" => Add
      case "-" => Subtract
      case "show" => Show
      case "clear" => Clear
      case _ => throw new IllegalOperation
    }

  }

  object GroupOps {
    
    sealed trait GroupOp {
      def using(a: Group, b: Group): Group
    }

    case object Promote extends GroupOp {
      def using(a: Group, b: Group) = a.promote
    }
    case object Demote extends GroupOp {
      def using(a: Group, b: Group) = a.demote
    }
    case object Set extends GroupOp {
      def using(a: Group, b: Group) = b
    }
    case object Show extends GroupOp {
      def using(a: Group, b: Group) = a
    }

    implicit val reader: Read[GroupOp] = Read.reads {
      case "promote" => Promote
      case "demote" => Demote
      case "set" => Set
      case "show" => Show
      case _ => throw new IllegalOperation
    }

  }

}
