package com.edawg878.bukkit

import com.edawg878.common.MessageFormatter
import org.bukkit.Bukkit
import org.bukkit.command.{Command, CommandExecutor, CommandSender}
import org.bukkit.entity.Player
import scopt.ConsoleHandler.ConsoleHandler
import scopt.{CustomOptionParser, Read}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
package object commands {

  private val handler: ConsoleHandler[CommandSender] = new ConsoleHandler[CommandSender] {

    override def print(sender: CommandSender, msg: String): Unit = sender.sendMessage(msg)

    override def exit(): Unit = {}

    override def printError(sender: CommandSender, msg: String): Unit = sender.sendMessage(MessageFormatter.ERROR + msg)
  }

  class BukkitOptionParser[C](cmd: String) extends CustomOptionParser[C, CommandSender](cmd)(handler)

  trait BukkitReaders {

    implicit val player: Read[Player] =
      Read.reads { s =>
        Option(Bukkit.getPlayerExact(s)) match {
          case Some(p) => p
          case name =>
            throw new IllegalArgumentException(s"'$s' is not online")
        }
      }

  }

  object Operation extends Enumeration {
    type Operation = Value
    val Add, Subtract, Set, Info = Value
  }

  trait NumericalOperations {

    import com.edawg878.bukkit.commands.Operation._

    implicit val operationReader: Read[Operation] =
      Read.reads {
        case "+" => Operation.Add
        case "-" => Operation.Subtract
        case "set" => Operation.Set
        case "show" => Operation.Info
        case _ => throw new IllegalArgumentException("Invalid Operation")
      }

    def calculate(x: Int, y: Int)(o: Operation): Int = o match {
      case Add => x + y
      case Subtract => x - y
      case Set => y
      case _ => x
    }

  }

  abstract class BukkitCommand[C] extends CommandExecutor {

    val default: C

    val parser: BukkitOptionParser[C]

    def handle(sender: CommandSender, c: C): Unit

    override def onCommand(sender: CommandSender, command: Command, label: String, args: Array[String]): Boolean = {
      parser.parse(args, default)(sender) match {
        case Some(c) =>
          handle(sender, c)
          true
        case _ => false
      }
    }

    def onComplete[A](sender: CommandSender, a: Future[A])(consumer: A => Unit): Unit = {
      a onComplete {
        case Success(result) =>
          consumer(result)
        case Failure(t) =>
          parser.reportError(t.getMessage)(sender)
      }
    }
  }

}
