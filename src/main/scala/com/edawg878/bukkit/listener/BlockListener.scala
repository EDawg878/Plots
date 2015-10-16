package com.edawg878.bukkit.listener

import com.edawg878.common.Server.{CustomReads, Configuration, Plugin}
import com.edawg878.common.Server.Configuration._
import org.bukkit.entity.Player
import org.bukkit.{Location, World, Material}
import org.bukkit.block.Block
import org.bukkit.event.{Cancellable, EventHandler, Listener}
import org.bukkit.event.block.BlockPlaceEvent
import com.edawg878.common.Color.Formatter
import org.bukkit.Material._
import play.api.libs.json.Json

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
case class BlockConfig(disableSnowman: Boolean,
                       disableIronGolem: Boolean,
                       disableWither: Boolean,
                       bannedPlace: Set[Material],
                       redstone: Set[Material])

trait Buildable {

  def isDisabled: Boolean

  def check(b: Block): Boolean

  def errorMessage: String

}

class Buildables(c: BlockConfig) {

  val buildables = Seq(snowman, ironGolem, wither)

  def find(ev: BlockPlaceEvent): Option[Buildable] =
    buildables.find(_.check(ev.getBlock))

  def snowman = new Buildable {

    val errorMessage = err"Snowmen are disabled"

    val isDisabled = c.disableSnowman

    def check(b: Block): Boolean =
      isPumpkin(b.getType) && checkSnowman(b.getLocation)

  }

  def ironGolem = new Buildable {

    val errorMessage = err"Iron golem are disabled"

    val isDisabled = c.disableIronGolem

    def check(b: Block): Boolean =
      isPumpkin(b.getType) && checkIronGolem(b.getLocation)

  }

  def wither = new Buildable {

    val errorMessage = err"Withers are disabled"

    val isDisabled = c.disableWither

    def check(b: Block): Boolean =
      isWitherSkull(b) && checkSnowman(b.getLocation)

  }

  def isPumpkin(mat: Material): Boolean =
    mat == PUMPKIN || mat == JACK_O_LANTERN

  def isWitherSkull(b: Block): Boolean =
    b.getType == SKULL && b.getData == 1

  def check(w: World, x: Int, y: Int, z: Int, mat: Material): Boolean =
    w.getBlockAt(x, y, z).getType == mat

  def wither(w: World, x: Int, y: Int, z: Int): Boolean =
    isWitherSkull(w.getBlockAt(x, y, z))

  def checkSnowman(loc: Location): Boolean = {
    val m  = SNOW_BLOCK
    val w = loc.getWorld
    val (x, y, z) = (loc.getBlockX, loc.getBlockY, loc.getBlockZ)
    check(w, x, y - 1, z, m) && check(w, x, y - 2, z, m)
  }

  def checkIronGolem(loc: Location): Boolean = {
    val m = IRON_BLOCK
    val w = loc.getWorld
    val (x, y, z) = (loc.getBlockX, loc.getBlockY, loc.getBlockZ)
    (check(w, x, y - 1, z, m) && check(w, x, y - 2, z, m)) && ((check(w, x + 1, y - 1, z, m) && check(w, x - 1, y - 1, z, m)) || ((check(w, x, y - 1, z + 1, m) && check(w, x, y - 1, z - 1, m))))
  }

  def checkWither(loc: Location): Boolean = {
    val m = SOUL_SAND
    val w = loc.getWorld
    val (x, y, z) = (loc.getBlockX, loc.getBlockY, loc.getBlockZ)
    check(w, x, y - 1, z, m) && ((check(w, x, y - 2, z, m) && ((wither(w, x + 1, y, z) && wither(w, x - 1, y, z) && check(w, x + 1, y - 1, z, m) && check(w, x - 1, y - 1, z, m)) || (wither(w, x, y, z + 1) && wither(w, x, y, z - 1) && check(w, x, y - 1, z + 1, m) && check(w, x, y - 1, z - 1, m)))) || (wither(w, x, y, z + 1) && wither(w, x, y, z + 2) && check(w, x, y - 1, z + 1, m) && check(w, x, y - 1, z + 2, m)) || (wither(w, x, y, z - 1) && wither(w, x, y, z - 2) && check(w, x, y - 1, z - 1, m) && check(w, x, y - 1, z - 2, m)) || (wither(w, x + 1, y, z) && wither(w, x + 2, y, z) && check(w, x + 1, y - 1, z, m) && check(w, x + 2, y - 1, z, m)) || (wither(w, x - 1, y, z) && wither(w, x - 2, y, z) && check(w, x - 1, y - 1, z, m) && check(w, x - 2, y - 1, z, m)))
  }

}


object BlockListener extends CustomReads {

  implicit val blockConfigReads = Json.format[BlockConfig]

  def load(p: Plugin): BlockListener = {
    val c = new Configuration[BlockConfig](p, "blocks.json")
    c.saveDefault()
    val blockConfig = c.parse
    val buildables = new Buildables(blockConfig)
    new BlockListener(blockConfig, buildables)
  }

}

class BlockListener(c: BlockConfig, b: Buildables) extends Listener {

  @EventHandler
  def onBlockPlace(ev: BlockPlaceEvent): Unit = {
    val p = ev.getPlayer
    val mat = ev.getBlock.getType
    if (c.bannedPlace.contains(mat)) {
      ev.setCancelled(true)
      p.sendMessage(err"This item is banned")
    } else if (c.redstone.contains(mat)) {
      // TODO
    } else {
      b.find(ev).foreach { bld =>
        p.sendMessage(bld.errorMessage)
        ev.setCancelled(true)
      }
    }

  }

}
