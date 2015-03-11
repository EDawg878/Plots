package com.edawg878.bukkit

import com.edawg878.bukkit.BukkitImpl._
import com.edawg878.common.Database._
import com.edawg878.common.Logging
import com.edawg878.common.Logging.PluginLogging
import org.bukkit.event.player.PlayerJoinEvent
import org.bukkit.event.{EventHandler, Listener}
import org.bukkit.plugin.java.JavaPlugin
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
class BukkitMain extends JavaPlugin with Listener with PluginLogging {

  Logging.log = getLogger
  val db = new MongoPlayerRepository

  override def onEnable() {
    getServer.getPluginManager.registerEvents(this, this)
  }

  @EventHandler
  def onInteract(event: PlayerJoinEvent) {
    val player = event.getPlayer
    db.find(player) onComplete {
      case Success(v) => logger.info("success!")
      case e => logger.info(s"failure! $e")
    }
  }

}
