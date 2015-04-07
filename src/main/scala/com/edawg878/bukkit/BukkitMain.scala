package com.edawg878.bukkit

import akka.actor.{ActorSystem, Props}
import akka.cluster.Cluster
import com.edawg878.bukkit.BukkitConversions._
import com.edawg878.common.Command
import com.edawg878.common.Modules.BukkitModule
import com.edawg878.common.{Publisher, Subscriber}
import org.bukkit.command.{CommandSender, CommandExecutor}
import org.bukkit.event.player.{PlayerQuitEvent, PlayerJoinEvent}
import org.bukkit.event.{EventHandler, Listener}
import org.bukkit.plugin.java.JavaPlugin

import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.JavaConverters._

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
class BukkitMain extends JavaPlugin with Listener {

  val bukkitModule = new BukkitModule {
    override def bukkitPlugin = BukkitMain.this
  }

  import bukkitModule._

  override def onLoad() {
    db.ensureIndexes()
    val systemName = "Test"
    val system = ActorSystem(systemName)
    val joinAddress = Cluster(system).selfAddress
    Cluster(system).join(joinAddress)
    system.actorOf(Props[Subscriber], "subscriber1")
    val publisher = system.actorOf(Props[Publisher], "publisher1")
    for (i <- 1 to 100) {
      publisher ! "hello"
    }
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

  def registerCommands(): Unit = {
    val commands = Seq(tierCommand, perkCommand, creditCommand, groupCommand, playTimeCommand, seenCommand)
    commands.foreach(registerCommand)
  }

  override def onEnable() {
    getServer.getPluginManager.registerEvents(this, this)
    registerCommands()
  }

  @EventHandler
  def onJoin(event: PlayerJoinEvent) {
    val player = event.getPlayer.toPlayer
    db.find(player) onSuccess {
      case data =>
        val updated = data.copy(playTime = data.playTime.login)
        db.save(updated)
    }
  }

  @EventHandler
  def onQuit(event: PlayerQuitEvent): Unit = {
    val player = event.getPlayer.toPlayer
    db.find(player) onSuccess {
      case data =>
        val updated = data.copy(playTime = data.playTime.logout)
        db.save(updated)
    }
  }

}
