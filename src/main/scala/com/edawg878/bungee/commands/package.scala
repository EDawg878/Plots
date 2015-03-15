package com.edawg878.bungee

import com.edawg878.common.MessageFormatter
import net.md_5.bungee.api.CommandSender
import net.md_5.bungee.api.chat.TextComponent
import scopt.ConsoleHandler.ConsoleHandler
import scopt.CustomOptionParser

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

  class BukkitOptionParser[C](cmd: String) extends CustomOptionParser[C, CommandSender](cmd)(handler)

}
