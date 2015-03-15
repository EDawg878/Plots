package com.edawg878.bukkit

import org.bukkit.ChatColor
import org.bukkit.command.CommandSender
import scopt.ConsoleHandler.ConsoleHandler
import scopt.CustomOptionParser

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
package object commands {

  val handler: ConsoleHandler[CommandSender] = new ConsoleHandler[CommandSender] {

    override def print(sender: CommandSender, msg: String): Unit = sender.sendMessage(msg)

    override def exit(): Unit = {}

    override def printError(sender: CommandSender, msg: String): Unit = sender.sendMessage(ChatColor.RED + msg)
  }

  class BukkitOptionParser[C](cmd: String) extends CustomOptionParser[C, CommandSender](cmd)(handler)

}
