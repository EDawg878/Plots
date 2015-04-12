package com.edawg878.common

import com.edawg878.common.Readers.PlayerDataReader
import scopt.ConsoleHandler.ConsoleHandler
import scopt.CustomOptionParser

import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
trait ConfigCommand[C, S] extends ErrorConverter[S] {

  def default: C

  def parser: CustomOptionParser[C, S]

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

object BukkitCommandHandler {

  import org.bukkit.command.CommandSender

  val console = new ConsoleHandler[CommandSender] {

    override def print(sender: CommandSender, msg: String): Unit =
      sender.sendMessage(msg)

    override def exit(): Unit = {}

    override def printError(sender: CommandSender, msg: String): Unit =
      sender.sendMessage(Color.Error + msg)
  }

  class BukkitOptionParser[C](cmd: String) extends CustomOptionParser[C, CommandSender](cmd)(console)

  abstract class BukkitCommand[C] extends BaseCommand[C, CommandSender]

  abstract class BasicBukkitCommand extends BasicCommand[CommandSender](console)

}

object BungeeCommandHandler {

  import net.md_5.bungee.api.CommandSender
  import net.md_5.bungee.api.chat.TextComponent

  val console = new ConsoleHandler[CommandSender] {
    override def print(sender: CommandSender, msg: String): Unit =
      sender.sendMessage(TextComponent.fromLegacyText(msg): _*)

    override def exit(): Unit = {}

    override def printError(sender: CommandSender, msg: String): Unit =
      sender.sendMessage(TextComponent.fromLegacyText(Color.Error + msg): _*)
  }

  class BungeeOptionParser[C](cmd: String) extends CustomOptionParser[C, CommandSender](cmd)(console)

  abstract class BungeeCommand[C] extends BaseCommand[C, CommandSender]

  abstract class BasicBungeeCommand extends BasicCommand[CommandSender](console)

}