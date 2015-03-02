package com.edawg878.bukkit

import com.edawg878.common.Configuration
import com.edawg878.common.MessageFormatter._
import com.edawg878.bukkit.BukkitImpl._
import org.bukkit.event.player.PlayerInteractEvent
import org.bukkit.event.{EventHandler => BukkitEvent, Listener}
import org.bukkit.plugin.java.JavaPlugin

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
class BukkitMain extends JavaPlugin with Listener {

  override def onEnable() {
    getServer.getPluginManager.registerEvents(this, this)
    val config = new BukkitConfiguration(this, "config.yml")
    config.saveDefault()
    test(config)
  }

  def test(config: Configuration): Unit = {
    config.set("test.node", true)
    config.save()
  }

  @BukkitEvent
  def onInteract(event: PlayerInteractEvent) {
    val player = event.getPlayer
    val hand = player.getItemInHand
    val num = player.getInventory
      .getContents
      .filter(_ != null)
      .count(_.getType == hand.getType)
    val name = event.getItem.getType.name
      .toLowerCase
      .split("_")
      .map(_.capitalize)
      .mkString(" ")
    player.sendMessage(fmt"You have [{0}] {0,choice,0#items|1#item|1<items} of type [{1}] ".info(num, name))
    player.sendMessage(info"This is an info message number = [$num]")
    player.sendMessage(err"This is an error message name = $name")
  }

}
