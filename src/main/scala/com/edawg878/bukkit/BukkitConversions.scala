package com.edawg878.bukkit

import java.io.InputStream
import java.nio.file.Path
import java.util.UUID
import java.util.logging.Logger

import com.edawg878.bukkit.plot.Position
import com.edawg878.common.Server._
import org.bukkit.scheduler.BukkitTask

import scala.concurrent.duration._

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object BukkitConversions {

  implicit def funcToRunnable(f: => Unit) = new Runnable { def run() = f }

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

    def toTask(t: BukkitTask): Task = new Task { def cancel() = t.cancel() }

    def toServer(p: org.bukkit.plugin.Plugin): Server = new Server {
      def getPlayer(name: String): Option[Player] = Option(s.getPlayer(name)).map(_.toPlayer)

      def getPlayer(id: UUID): Option[Player] = Option(s.getPlayer(id)).map(_.toPlayer)

      def sync(f: Runnable, delay: Long): Unit = s.getScheduler.runTaskLater(p, f, delay)

      def async(f: => Unit, delay: Long): Unit = s.getScheduler.runTaskLaterAsynchronously(p, f, delay)

      def schedule(d: Duration, delay: Long, f: => Unit): Task =
        toTask(s.getScheduler.runTaskTimer(p, f, delay, d.toTicks))

      def scheduleAsync(d: Duration, delay: Long, f: => Unit) =
        toTask(s.getScheduler.runTaskTimerAsynchronously(p, f, delay, d.toTicks))

      def shutdown(): Unit = s.shutdown()

    }

  }

  implicit class BukkitLocation(l: org.bukkit.Location) {

    def toPosition: Position = Position(l.getBlockX, l.getBlockY, l.getBlockZ)

  }

}
