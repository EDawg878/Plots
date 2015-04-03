package com.edawg878.bukkit

import com.edawg878.common.Command.ConfigCommand
import com.edawg878.common.MessageFormatter
import org.bukkit.command.{Command, CommandExecutor, CommandSender}
import scopt.ConsoleHandler.ConsoleHandler
import scopt.CustomOptionParser

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
package object commands {

  implicit val handler: ConsoleHandler[CommandSender] = new ConsoleHandler[CommandSender] {

    override def print(sender: CommandSender, msg: String): Unit = sender.sendMessage(msg)

    override def exit(): Unit = {}

    override def printError(sender: CommandSender, msg: String): Unit = sender.sendMessage(MessageFormatter.ERROR + msg)
  }

  class BukkitOptionParser[C](cmd: String) extends CustomOptionParser[C, CommandSender](cmd)

  abstract class BukkitCommand[C] extends ConfigCommand[C, CommandSender] with CommandExecutor {

    override def onCommand(sender: CommandSender, command: Command, label: String, args: Array[String]): Boolean = {
      run(sender, args)
    }

  }

}
