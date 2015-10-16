package com.edawg878.bukkit

import java.io.InputStream
import java.nio.file.Path
import java.util.UUID
import java.util.logging.Logger

import com.edawg878.bukkit.plot.Position
import com.edawg878.common.Server._

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object BukkitConversions {

  implicit class BukkitPlugin(plugin: org.bukkit.plugin.Plugin) {

    def toPlugin: Plugin = new Plugin {
      val dataFolder: Path = plugin.getDataFolder.toPath

      val logger: Logger = plugin.getLogger

      def getResource(name: String): InputStream = plugin.getResource(name)
    }

  }

  implicit class BukkitConsole(console: org.bukkit.command.CommandSender) {

    def toConsole: Console = new Console {
      def sendMessage(message: String): Unit = console.sendMessage(message)

      def hasPermission(permission: String): Boolean = console.hasPermission(permission)
    }

  }

  implicit class BukkitPlayer(player: org.bukkit.entity.Player) {

    def toPlayer: Player = new Player {
      def name: String = player.getName

      def id: UUID = player.getUniqueId

      def sendMessage(message: String): Unit = player.sendMessage(message)

      def hasPermission(permission: String): Boolean = player.hasPermission(permission)

      def getDisplayName: String = player.getDisplayName
    }

  }

  //implicit def bukkitPlayerToUniqueId(p: org.bukkit.entity.Player): UUID = p.getUniqueId

  implicit class BukkitServer(s: org.bukkit.Server) {

    def toServer(p: org.bukkit.plugin.Plugin): Server = new Server {
      def getPlayer(name: String): Option[Player] = Option(s.getPlayer(name)).map(_.toPlayer)

      def getPlayer(id: UUID): Option[Player] = Option(s.getPlayer(id)).map(_.toPlayer)

      def sync(f: => Unit): Unit = s.getScheduler.scheduleSyncDelayedTask(p, new Runnable { def run(): Unit = f })
    }

  }

  implicit class BukkitLocation(l: org.bukkit.Location) {

    def toPosition: Position = Position(l.getBlockX, l.getBlockY, l.getBlockZ)

  }

}
