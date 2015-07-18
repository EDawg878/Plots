package com.edawg878.bukkit.commands

import com.edawg878.common.BukkitCommandHandler.{BukkitOptionParser, BukkitCommand}
import com.edawg878.common.Modules.BukkitModule
import com.edawg878.common.{CommandMeta, IllegalOperation}
import org.bukkit.command.CommandSender
import scopt.Read

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Admin {

  sealed trait AdminOp

  case object Reload extends AdminOp

  implicit val adminOpsReader: Read[AdminOp] = Read.reads {
    case "reload" => Reload
    case _ => throw new IllegalOperation
  }

  case class Config(op: AdminOp)

  class AdminCommand(m: BukkitModule) extends BukkitCommand[Config] {
    val meta = CommandMeta(cmd = "admin", perm = None)

    val default = Config(op = Reload)

    val parser = new BukkitOptionParser[Config]("/admin") {
      arg[AdminOp]("<operation>") required() action { (x,c) =>
        c.copy(op = x)
      } text "operations: reload"
    }

    def handle(sender: CommandSender, c: Config): Unit = {
      c.op match {
        case Reload =>
          //m.config = m.loadConfig
      }
    }
  }

}
