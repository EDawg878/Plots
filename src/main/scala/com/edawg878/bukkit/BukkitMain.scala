package com.edawg878.bukkit

import com.edawg878.bukkit.listener.VehicleTracker
import com.edawg878.bukkit.plot.PlotGenerator
import com.edawg878.common.Server.CustomReads
import com.edawg878.common.{PlayTime, PlayerNotFound, PlayerData, Command}
import com.edawg878.common.Modules.BukkitModule
import org.bukkit.command.{CommandExecutor, CommandSender}
import org.bukkit.event.player.{PlayerJoinEvent, PlayerQuitEvent}
import org.bukkit.event.{EventHandler, Listener}
import org.bukkit.generator.ChunkGenerator
import org.bukkit.plugin.java.JavaPlugin
import BukkitConversions._

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.Success

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
class BukkitMain extends JavaPlugin with Listener {

  val bukkitModule = new BukkitModule {
    override def bukkitPlugin = BukkitMain.this
  }

  import bukkitModule._

  override def onLoad(): Unit = {
    databases.foreach(_.ensureIndexes())
  }

  def registerCommand(command: Command[CommandSender]): Unit = {
    val meta = command.meta
    val exec = new CommandExecutor {
      override def onCommand(sender: CommandSender, bcmd: org.bukkit.command.Command, l: String, args: Array[String]): Boolean = {
        command.execute(sender, args)
        true
      }
    }
    val bc = getCommand(meta.cmd)
    meta.perm.foreach(bc.setPermission)
    bc.setAliases(meta.aliases.asJava)
    bc.setExecutor(exec)
  }

  def registerListener(l: Listener): Unit = getServer.getPluginManager.registerEvents(l, this)

  override def onEnable(): Unit = {
    getServer.getPluginManager.registerEvents(this, this)
    listeners.foreach(registerListener)
    commands.foreach(registerCommand)
  }

  override def onDisable(): Unit = {
    saveVehicles()
  }

  def saveVehicles(): Unit = {
    vehicleTrackers.foreach(_.save(plugin))
  }

  override def getDefaultWorldGenerator(worldName: String, style: String): ChunkGenerator =
    getPlotWorldConfig(worldName).map(new PlotGenerator(_)).orNull

  @EventHandler
  def onJoin(ev: PlayerJoinEvent) {
    val p = ev.getPlayer
    playerDb.search(p.getUniqueId)
      .map(_.getOrElse(new PlayerData(p.getUniqueId, p.getName)))
      .map(_.updateName(p.getName).login)
      .foreach { d =>
        playerDb.save(d)
        playerCache.update(d)
      }
  }

  @EventHandler
  def onQuit(ev: PlayerQuitEvent): Unit = {
    val p = ev.getPlayer
    playerDb.find(p.getUniqueId) map { d =>
      playerDb.save(d)
      playerCache.update(d)
    }
  }

}
