package com.edawg878.bungee

import java.io.InputStream
import java.nio.file.Path
import java.util.UUID
import java.util.logging.Logger

import com.edawg878.common.Server._
import net.md_5.bungee.api.ProxyServer
import net.md_5.bungee.api.chat.TextComponent
import net.md_5.bungee.api.connection.ProxiedPlayer

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object BungeeConversions {

  implicit class BungeePlugin(val plugin: net.md_5.bungee.api.plugin.Plugin) {

    def toPlugin: Plugin = new Plugin {
      override val dataFolder: Path = plugin.getDataFolder.toPath

      override val logger: Logger = plugin.getLogger

      override def getResource(name: String): InputStream = plugin.getResourceAsStream(name)
    }

  }

  implicit class BungeeConsole(val console: net.md_5.bungee.api.CommandSender)  {

    def toConsole: Console = new Console {
      override def sendMessage(message: String): Unit = console.sendMessage(message)

      override def hasPermission(permission: String): Boolean = console.hasPermission(permission)
    }

  }

  implicit class BungeePlayer(val player: ProxiedPlayer) {

    def toPlayer: Player = new Player {
      override def name: String = player.getName

      override def id: UUID = player.getUniqueId

      override def sendMessage(message: String): Unit = player.sendMessage(TextComponent.fromLegacyText(message): _*)

      override def hasPermission(permission: String): Boolean = player.hasPermission(permission)

      override def getDisplayName: String = player.getDisplayName
    }

  }

  implicit class BungeeServer(val server: ProxyServer) {

    def toServer: Server = new Server {
      override def getPlayer(name: String): Option[Player] = Option(server.getPlayer(name)).map(_.toPlayer)

      override def getPlayer(id: UUID): Option[Player] = Option(server.getPlayer(id)).map(_.toPlayer)
    }

  }

}
