package com.edawg878.bungee

import com.edawg878.common.Command.{ConfigCommand, ErrorConverter}
import com.edawg878.common.MessageFormatter
import net.md_5.bungee.api.connection.ProxiedPlayer
import net.md_5.bungee.api.{ProxyServer, CommandSender}
import net.md_5.bungee.api.chat.TextComponent
import net.md_5.bungee.api.plugin.Command
import scopt.ConsoleHandler.ConsoleHandler
import scopt.{Read, CustomOptionParser}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
package object commands {

  val handler: ConsoleHandler[CommandSender] = new ConsoleHandler[CommandSender] {

    override def print(sender: CommandSender, msg: String): Unit =
      sender.sendMessage(TextComponent.fromLegacyText(msg): _*)

    override def exit(): Unit = {}

    override def printError(sender: CommandSender, msg: String): Unit =
      sender.sendMessage(TextComponent.fromLegacyText(MessageFormatter.ERROR + msg): _*)
  }

  class BungeeOptionParser[C](cmd: String) extends CustomOptionParser[C, CommandSender](cmd)(handler)

  trait BungeeReaders {

    implicit val player: Read[ProxiedPlayer] =
      Read.reads { s =>
        Option(server.getPlayer(s)) match {
          case Some(p) => p
          case name =>
            throw new IllegalArgumentException(s"'$s' is not online")
        }
      }

    def server: ProxyServer

  }

  abstract class BungeeCommand[C](cmd: String) extends Command(cmd) with ConfigCommand[C, CommandSender] {

    override def execute(sender: CommandSender, args: Array[String]): Unit = {
      run(sender, args)
    }

  }

}
