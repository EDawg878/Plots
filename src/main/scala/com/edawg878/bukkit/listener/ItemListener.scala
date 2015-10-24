package com.edawg878.bukkit.listener

import com.edawg878.common.Server.{CustomReads, Configuration, Plugin}
import org.bukkit.Material
import org.bukkit.event.block.Action
import org.bukkit.event.{EventHandler, Listener}
import org.bukkit.event.player.PlayerInteractEvent
import org.bukkit.inventory.ItemStack
import org.bukkit.potion.{Potion, PotionEffectType}
import play.api.libs.json.Json
import scala.collection.JavaConversions._
import com.edawg878.common.Color.Formatter

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
case class ItemConfig(bannedInteract: Set[Material],
                      pvpOnly: Set[Material],
                      dispenserFilter: Set[Material],
                      bannedPotionEffectTypes: Set[String])

object ItemListener extends CustomReads {

  implicit val itemConfigReads = Json.format[ItemConfig]

  def load(p: Plugin): ItemListener = {
    val c = new Configuration[ItemConfig](p, "items.json")
    c.saveDefault()
    new ItemListener(c.parse)
  }

}

class ItemListener(c: ItemConfig) extends Listener {

  def isWaterPotion(itm: ItemStack): Boolean =
    getPotionEffectBits(itm) == 0

  def getPotionEffectBits(itm: ItemStack): Int =
    itm.getDurability & 0x3F

  def isPotionBlocked(itm: ItemStack): Boolean = {
    if (itm.getType != Material.POTION || !isWaterPotion(itm)) false
    else Potion.fromDamage(getPotionEffectBits(itm))
        .getEffects
        .map(_.getType.getName)
        .exists(c.bannedPotionEffectTypes.contains)
  }

  @EventHandler
  def onPlayerInteract(ev: PlayerInteractEvent): Unit =
    if (ev.getAction == Action.RIGHT_CLICK_BLOCK || ev.getAction == Action.RIGHT_CLICK_AIR) {
      Option(ev.getItem).foreach { itm =>
        val p = ev.getPlayer
        val mat = itm.getType
        if (c.bannedInteract.contains(mat) || isPotionBlocked(itm)) {
          ev.setCancelled(true)
          p.sendMessage(err"This item is banned")
        } else if (c.pvpOnly.contains(mat) && false /* TODO !plugin.isPvP(p) */) {
          ev.setCancelled(true)
          p.sendMessage(err"This item can only be used in PvP mode")
        }
      }
    }

}
