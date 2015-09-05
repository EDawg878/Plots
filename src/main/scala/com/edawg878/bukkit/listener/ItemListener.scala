package com.edawg878.bukkit.listener

import com.edawg878.common.Server.{Configuration, Plugin}
import org.bukkit.Material
import org.bukkit.event.block.Action
import org.bukkit.event.{EventHandler, Listener}
import org.bukkit.event.player.PlayerInteractEvent
import org.bukkit.inventory.ItemStack
import org.bukkit.potion.{Potion, PotionEffectType}
import scala.collection.JavaConverters._
import com.edawg878.common.Color.Formatter

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object ItemListener {

  case class ItemConfig(bannedInteract: Set[Material],
                         pvpOnly: Set[Material],
                         dispenserFilter: Set[Material],
                         bannedPotionEffectTypes: Set[PotionEffectType])

  def load(p: Plugin): ItemListener = {
    val blockConfig = new Configuration[ItemConfig](p, "item.json").parse
    new ItemListener(blockConfig)
  }

  class ItemListener(c: ItemConfig) extends Listener {

    def isWaterPotion(itm: ItemStack): Boolean =
      getPotionEffectBits(itm) == 0

    def getPotionEffectBits(itm: ItemStack): Int =
      itm.getDurability & 0x3F

    def isPotionBlocked(itm: ItemStack): Boolean = {
      if (itm.getType != Material.POTION || !isWaterPotion(itm)) false
      else Potion.fromDamage(getPotionEffectBits(itm))
          .getEffects.asScala
          .map(_.getType).exists(c.bannedPotionEffectTypes.contains)
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

}
