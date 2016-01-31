package com.edawg878.bukkit

import java.util

import com.edawg878.bukkit.plot.PlotCommand.{EDawgPlotLimitChecker, DefaultPlotLimitChecker}
import com.edawg878.bukkit.plot.{WorldEditListener, PlotGenerator}
import com.edawg878.common.Modules.BukkitModule
import com.edawg878.common.{Command, PlayerData}
import org.bukkit.command.{CommandExecutor, CommandSender}
import org.bukkit.event.player.{PlayerJoinEvent, PlayerQuitEvent}
import org.bukkit.event.server.{PluginDisableEvent, PluginEnableEvent}
import org.bukkit.event.world.WorldLoadEvent
import org.bukkit.event.{HandlerList, EventHandler, Listener}
import org.bukkit.generator.ChunkGenerator
import org.bukkit.plugin.java.JavaPlugin

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.JavaConversions._

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

  def getPlayersInPlot(p: org.bukkit.entity.Player): java.util.List[org.bukkit.entity.Player] = {
    val list = new util.ArrayList[org.bukkit.entity.Player]()
    val resolver = bukkitPlotWorldResolver
    resolver(p.getWorld).foreach { w =>
      val srcId = w.getPlotId(p.getLocation)
      for (u <- p.getWorld.getPlayers) {
        if (w.getPlotId(u.getLocation) == srcId)
          list.add(u)
      }
    }
    list
  }

  def isTrusted(p: org.bukkit.entity.Player): java.lang.Boolean = {
    val resolver = bukkitPlotWorldResolver
    if (resolver(p.getWorld).flatMap(w => w.getPlot(w.getPlotId(p.getLocation))).exists(_.isTrusted(p.getUniqueId)))
      java.lang.Boolean.TRUE
    else
      java.lang.Boolean.FALSE
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
  }

  override def getDefaultWorldGenerator(worldName: String, style: String): ChunkGenerator =
    getPlotWorldConfig(worldName).map(new PlotGenerator(_)).orNull

  @EventHandler
  def onWorldLoad(ev: WorldLoadEvent): Unit = {
    setWorldBorder(ev.getWorld)
  }

  @EventHandler
  def onPluginEnable(ev: PluginEnableEvent): Unit = {
    val plugin = ev.getPlugin
    plugin.getName match {
      case "EDawg878-Core" =>
        plotLimitChecker = new EDawgPlotLimitChecker
        getLogger.info("Connected to EDawg878-Core")
      case "WorldEdit" =>
        val listener = WorldEditListener.create(bukkitPlotWorldResolver, server, plugin, worldEditConfig)
        worldEditListener = Some(listener)
        registerListener(listener)
        getLogger.info("Connected to WorldEdit")
      case _ =>
    }
  }

  @EventHandler
  def onPluginDisable(ev: PluginDisableEvent): Unit = {
    val plugin = ev.getPlugin
    plugin.getName match {
      case "EDawg878-Core" =>
        plotLimitChecker = new DefaultPlotLimitChecker(playerDb)
        registerCommand(plotCommand)
        getLogger.info("Disconnected from EDawg878-Core")
      case "WorldEdit" =>
        worldEditListener.foreach(HandlerList.unregisterAll)
        worldEditListener = None
        getLogger.info("Disconnected from WorldEdit")
      case _ =>
    }
  }

  @EventHandler
  def onJoin(ev: PlayerJoinEvent): Unit = {
    val p = ev.getPlayer
    playerDb.search(p.getUniqueId)
      .map(_.getOrElse(new PlayerData(p.getUniqueId, p.getName)))
      .map(_.updateName(p.getName).login)
      .foreach(playerDb.save)
  }

  @EventHandler
  def onQuit(ev: PlayerQuitEvent): Unit = {
    val p = ev.getPlayer
    playerDb.find(p.getUniqueId)
            .foreach(playerDb.save)
  }

}
