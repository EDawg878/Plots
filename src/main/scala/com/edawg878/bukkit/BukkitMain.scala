package com.edawg878.bukkit

import akka.actor.{ActorSystem, Props}
import akka.cluster.Cluster
import com.edawg878.bukkit.BukkitImpl._
import com.edawg878.common.Modules.BukkitModule
import com.edawg878.common.{Publisher, Subscriber}
import org.bukkit.event.player.PlayerJoinEvent
import org.bukkit.event.{EventHandler, Listener}
import org.bukkit.plugin.java.JavaPlugin

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

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

  override def onEnable() {
    getServer.getPluginManager.registerEvents(this, this)
    getCommand("tier").setExecutor(tierCommand)
    getCommand("perk").setExecutor(perkCommand)
    getCommand("credit").setExecutor(creditCommand)
    getCommand("group").setExecutor(groupCommand)
  }

  @EventHandler
  def onJoin(event: PlayerJoinEvent) {
    val player = event.getPlayer
    db.find(player) onComplete {
      case Success(v) => getLogger.info("success!")
      case Failure(t) => { getLogger.info(s"failure! ${t.getMessage}"); db.insert(player) }
    }
  }

}
