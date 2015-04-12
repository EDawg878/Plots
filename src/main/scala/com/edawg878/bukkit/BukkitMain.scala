package com.edawg878.bukkit

import java.nio.file.Files

import akka.actor.{ActorSystem, Props}
import akka.cluster.Cluster
import com.edawg878.bukkit.BukkitConversions._
import com.edawg878.common.{MyConfig, Command, Publisher, Subscriber}
import com.edawg878.common.Modules.BukkitModule
import com.edawg878.common.Server.Plugin
import org.bukkit.Location
import org.bukkit.command.{CommandSender, CommandExecutor}
import org.bukkit.event.player.{PlayerQuitEvent, PlayerJoinEvent}
import org.bukkit.event.{EventHandler, Listener}
import org.bukkit.plugin.java.JavaPlugin
import play.api.libs.json.{JsPath, Format, Json}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.JavaConverters._
import scala.io.Source

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
    /*
    val systemName = "Test"
    val system = ActorSystem(systemName)
    val joinAddress = Cluster(system).selfAddress
    Cluster(system).join(joinAddress)
    system.actorOf(Props[Subscriber], "subscriber1")
    val publisher = system.actorOf(Props[Publisher], "publisher1")
    for (i <- 1 to 100) {
      publisher ! "hello"
    }
    */
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
    /*
    val p = this.toPlugin
    val path = p.dataFolder.resolve("example")
    if (Files.notExists(path)) Files.createFile(path)

    import java.nio.charset.StandardCharsets.UTF_8
    val l = new Location(getServer.getWorlds.get(0), 1, 2, 3, 4, 5)
    val c = MyConfig(locs = List(l, l, l), l)
    val json = Json.prettyPrint(Json.toJson(c))
    Files.write(path, json.getBytes(UTF_8))
    */
    val l = new Location(getServer.getWorlds.get(0), 1, 2, 3, 4, 5)
    val name = "MyConfig"
    val c = MyConfig(name, List(l,l,l), l)
    cdb.save(c)
    cdb.find(name).collect {
      case Some(myc) => getLogger.info(myc.toString)
    }
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
