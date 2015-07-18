package com.edawg878.bukkit

import com.edawg878.bukkit.plot.PlotGenerator
import com.edawg878.common.{PlayerData, Command}
import com.edawg878.common.Modules.BukkitModule
import org.bukkit.command.{CommandExecutor, CommandSender}
import org.bukkit.event.player.{PlayerJoinEvent, PlayerQuitEvent}
import org.bukkit.event.{EventHandler, Listener}
import org.bukkit.generator.ChunkGenerator
import org.bukkit.plugin.java.JavaPlugin
import BukkitConversions._

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
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
    db.ensureIndexes()
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
    meta.perm.map(bc.setPermission)
    bc.setAliases(meta.aliases.asJava)
    bc.setExecutor(exec)
  }

  def registerListener(l: Listener): Unit = getServer.getPluginManager.registerEvents(l, this)

  override def onEnable(): Unit = {
    getServer.getPluginManager.registerEvents(this, this)
    listeners.foreach(registerListener)
    commands.foreach(registerCommand)
  }

  override def getDefaultWorldGenerator(worldName: String, style: String): ChunkGenerator =
    getPlotWorld(worldName).map(new PlotGenerator(_)).orNull

  @EventHandler
  def onJoin(event: PlayerJoinEvent) {
    val player = event.getPlayer.toPlayer
    db.find(player.id) onComplete {
      case Success(data) =>
        val updated = data.updateName(player.name)
          .copy(playTime = data.playTime.login)
        db.save(updated)
      case _ =>
        db.insert(new PlayerData(player))
    }
  }

  @EventHandler
  def onQuit(event: PlayerQuitEvent): Unit = {
    val player = event.getPlayer
    db.find(player.getUniqueId) onSuccess {
      case data =>
        val updated = data.copy(playTime = data.playTime.logout)
        db.save(updated)
    }
  }

}
