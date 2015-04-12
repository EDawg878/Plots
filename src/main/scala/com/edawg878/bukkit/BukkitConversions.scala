package com.edawg878.bukkit

import java.io.InputStream
import java.nio.file.Path
import java.util.UUID
import java.util.logging.Logger

import com.edawg878.common.Server._

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object BukkitConversions {

  implicit class BukkitPlugin(val plugin: org.bukkit.plugin.Plugin) {

    def toPlugin: Plugin = new Plugin {
      override val dataFolder: Path = plugin.getDataFolder.toPath

      override val logger: Logger = plugin.getLogger

      override def getResource(name: String): InputStream = plugin.getResource(name)
    }

  }

  implicit class BukkitConsole(val console: org.bukkit.command.CommandSender) {

    def toConsole: Console = new Console {
      override def sendMessage(message: String): Unit = console.sendMessage(message)

      override def hasPermission(permission: String): Boolean = console.hasPermission(permission)
    }

  }

  implicit class BukkitPlayer(val player: org.bukkit.entity.Player) {

    def toPlayer: Player = new Player {
      override def name: String = player.getName

      override def id: UUID = player.getUniqueId

      override def sendMessage(message: String): Unit = player.sendMessage(message)

      override def hasPermission(permission: String): Boolean = player.hasPermission(permission)

      override def getDisplayName: String = player.getDisplayName
    }

  }

  implicit class BukkitServer(val server: org.bukkit.Server) {

    def toServer: Server = new Server {
      override def getPlayer(name: String): Option[Player] = Option(server.getPlayer(name)).map(_.toPlayer)

      override def getPlayer(id: UUID): Option[Player] = Option(server.getPlayer(id)).map(_.toPlayer)
    }

  }

}
