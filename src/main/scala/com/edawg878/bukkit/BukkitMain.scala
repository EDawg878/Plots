package com.edawg878.bukkit

import com.edawg878.bukkit.BukkitImpl._
import com.edawg878.common.Modules.Module
import com.edawg878.common.Plugin
import org.bukkit.event.player.PlayerJoinEvent
import org.bukkit.event.{EventHandler, Listener}
import org.bukkit.plugin.java.JavaPlugin

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
class BukkitMain extends JavaPlugin with Listener {

  val modules = new Module {
    override def plugin: Plugin = BukkitMain.this
  }

  override def onLoad() {
    modules.db.ensureIndexes()
  }

  override def onEnable() {
    getServer.getPluginManager.registerEvents(this, this)
    getCommand("tier").setExecutor(modules.tierCommand)
    getCommand("perk").setExecutor(modules.perkCommand)
    getCommand("credit").setExecutor(modules.creditCommand)
  }

  @EventHandler
  def onJoin(event: PlayerJoinEvent) {
    val player = event.getPlayer
    modules.db.find(player) onComplete {
      case Success(v) => getLogger.info("success!")
      case e => getLogger.info(s"failure! $e")
    }
  }

}
