package com.edawg878.bukkit.plot

import com.edawg878.bukkit.plot.Plot._
import com.edawg878.common.Color.Formatter
import com.edawg878.common.PlotRepository
import com.edawg878.common.Server.Server
import org.bukkit.Difficulty._
import org.bukkit._
import org.bukkit.block.Block
import org.bukkit.enchantments.Enchantment
import org.bukkit.entity.EntityType._
import org.bukkit.entity._
import org.bukkit.event.EventPriority._
import org.bukkit.event._
import org.bukkit.event.block.Action._
import org.bukkit.event.block._
import org.bukkit.event.enchantment.EnchantItemEvent
import org.bukkit.event.entity.{EntityChangeBlockEvent, EntityDamageByEntityEvent, EntityExplodeEvent}
import org.bukkit.event.hanging.{HangingBreakByEntityEvent, HangingPlaceEvent}
import org.bukkit.event.inventory.PrepareAnvilEvent
import org.bukkit.event.player._
import org.bukkit.event.vehicle.VehicleDestroyEvent
import org.bukkit.event.world.{StructureGrowEvent, WorldLoadEvent}

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
class PlotListener(val resolver: PlotWorldResolver, plotDb: PlotRepository, val server: Server, val bukkitServer: org.bukkit.Server) extends Listener with PlotHelper {

  val defaultStatus = HelperOnline
  val defaultError = err"You cannot build here"

  type Result = Either[String, Unit]

  def test(p: Player, loc: Location): Result = test(p, loc, defaultStatus, defaultError)
  def resetExpiration(p: Player, loc: Location): Result = resetExpiration(p, loc, defaultStatus, defaultError)

  def test(p: Player, loc: Location, st: Status, s: String): Result = {
    val r = has(st, p, loc, s)
    r.left.foreach(p.sendMessage)
    r
  }

  def resetExpiration(p: Player, loc: Location, st: Status, s: String): Result = {
    val r = test(p, loc, st, s)
    r.right.foreach { _ =>
      resolver(loc.getWorld) foreach { w =>
        w.getPlot(w.getPlotId(loc))
          .flatMap(_.resetExpire)
          .foreach { plot =>
            w.update(plot)
            plotDb.save(plot)
          }
      }
    }
    r
  }

  def cancelLeft(ev: Cancellable, r: => Result): Unit = {
    if (!ev.isCancelled && r.isLeft) ev.setCancelled(true)
  }
  def cancel(ev: Cancellable, cond: => Boolean): Unit = {
    if (!ev.isCancelled && cond) ev.setCancelled(true)
  }

  def isInterplot(plotA: Plot, locA: Location, locB: Location): Boolean = {
    if (locA.getWorld != locB.getWorld || !isPlotWorld(locA)) false
    else {
      resolver(locB.getWorld).fold(false) { w =>
        val idB = w.getPlotId(locB)
        w.getPlot(idB).fold(true /* plot -> empty plot */) { plotB =>
          if (!plotA.roadAccess && w.config.isRoad(locA)) true // plot w/o road access -> road
          else if (!plotB.roadAccess && w.config.isRoad(locB)) true // plot w/o road access -> road
          else if (plotA.id == plotB.id) false // same plot
          else if (plotB.isTrusted(plotA.owner)) false // owner is trusted
          else plotA.owner != plotB.owner
        }
      }
    }
  }

  def testInterplot(locA: Location, locB: Location): Boolean =
    resolver(locA.getWorld).fold(false)(w => w.getPlot(w.getPlotId(locA)).fold(true)(isInterplot(_, locA, locB)))

  def testPistonExtend(ev: BlockPistonExtendEvent, bs: Seq[Block]): Boolean = {
    val locA = ev.getBlock.getLocation
    val face = ev.getDirection
    getPlot(locA).fold(false)(plot =>
      bs.map(_.getLocation.add(face.getModX, face.getModY, face.getModZ))
        .exists(isInterplot(plot, locA, _)))
  }

  def testPistonRetract(ev: BlockPistonRetractEvent, bs: Seq[Block]): Boolean = {
    val locA = ev.getBlock.getLocation
    getPlot(locA).fold(false)(plot =>
      bs.map(_.getLocation).exists(isInterplot(plot, locA, _)))
  }

  @EventHandler
  def onBlockBreak(ev: BlockBreakEvent): Unit =
    cancelLeft(ev, resetExpiration(ev.getPlayer, ev.getBlock.getLocation))

  @EventHandler
  def onBlockPlace(ev: BlockPlaceEvent): Unit =
    cancelLeft(ev, resetExpiration(ev.getPlayer, ev.getBlock.getLocation))

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onEntityChangeBlock(ev: EntityChangeBlockEvent): Unit = {
    val loc = ev.getBlock.getLocation
    if (isPlotWorld(loc)) {
      ev.getEntity match {
        case p: Player =>
          cancelLeft(ev, test(p, loc))
        case e if !e.isInstanceOf[FallingBlock] =>
          ev.setCancelled(true)
        case _ =>
      }
    }
  }

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onEntityBlockForm(ev: EntityBlockFormEvent): Unit = {
    ev.getEntity match {
      case p: Player =>
        cancelLeft(ev, test(p, ev.getBlock.getLocation))
      case _ =>
        ev.setCancelled(true)
    }
  }

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onPlayerBucketEmpty(ev: PlayerBucketEmptyEvent): Unit = {
    val face = ev.getBlockFace
    val loc = ev.getBlockClicked.getLocation
      .add(face.getModX, face.getModY, face.getModZ)
      .getBlock
      .getLocation
    cancelLeft(ev, test(ev.getPlayer, loc, Trusted, err"You must be trusted to the plot in order to empty buckets"))
  }

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onPlayerBucketFill(ev: PlayerBucketFillEvent): Unit =
    cancelLeft(ev, test(ev.getPlayer, ev.getBlockClicked.getLocation, Trusted, err"You must be trusted to the plot in order to fill buckets"))

  def touchedFire(p: Player): Boolean = {
//    val set: Set[Material] = null
//    Try(Option(p.getTargetBlock(set, 5)).exists(_.getType == Material.FIRE)).getOrElse(false)
    false
  }


  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onPlayerInteract(ev: PlayerInteractEvent): Unit = {
    inPlotWorld(ev.getPlayer) { w =>
      if (ev.hasBlock) {
        val clicked = ev.getClickedBlock
        if (ev.getAction == LEFT_CLICK_BLOCK && touchedFire(ev.getPlayer)) {
          cancelLeft(ev, test(ev.getPlayer, clicked.getLocation))
        } else if (ev.getAction == RIGHT_CLICK_BLOCK) {
          if (w.config.preventedItems.contains(ev.getMaterial)) {
            cancelLeft(ev, test(ev.getPlayer, clicked.getLocation, HelperOnline, err"You must be added to the plot in order to use this item"))
          }
          if (w.config.protectedBlocks.contains(clicked.getType)) {
            cancelLeft(ev, test(ev.getPlayer, clicked.getLocation, HelperOnline, err"You must be added to the plot in order to interact with this block"))
          }
        }
      }
    }
  }

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onBlockSpread(ev: BlockSpreadEvent): Unit =
    cancel(ev, testInterplot(ev.getBlock.getLocation, ev.getSource.getLocation))

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onBlockFade(ev: BlockFadeEvent): Unit =
    cancel(ev, isPlotWorld(ev.getBlock.getLocation))

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onBlockFromTo(ev: BlockFromToEvent): Unit =
    cancel(ev, testInterplot(ev.getBlock.getLocation, ev.getToBlock.getLocation))

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onBlockPistonExtend(ev: BlockPistonExtendEvent): Unit =
    cancel(ev, testPistonExtend(ev, ev.getBlocks.asScala))

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onBlockPistonRetract(ev: BlockPistonRetractEvent): Unit =
    cancel(ev, testPistonRetract(ev, ev.getBlocks.asScala))

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onPlayerStructureGrow(ev: StructureGrowEvent): Unit =
    cancel(ev, Option(ev.getPlayer).exists(test(_, ev.getLocation).isLeft))

  @EventHandler(priority = LOW, ignoreCancelled = true)
  def onStructureGrow(ev: StructureGrowEvent): Unit = {
    val locA = ev.getLocation
    getPlot(locA).fold(ev.setCancelled(true))(plot =>
      ev.getBlocks.removeAll(ev.getBlocks.asScala.filter(b => b != null && isInterplot(plot, locA, b.getLocation))))
  }

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onEntityExplode(ev: EntityExplodeEvent): Unit =
    Option(ev.getLocation).foreach(loc => cancel(ev, isPlotWorld(loc)))

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onBlockIgnite(ev: BlockIgniteEvent): Unit =
    Option(ev.getPlayer).fold(ev.setCancelled(true))(p =>
      cancelLeft(ev, test(p, ev.getBlock.getLocation, Trusted, err"You must be trusted to the plot in order to ignite blocks")))

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onHangingPlace(ev: HangingPlaceEvent): Unit =
    cancelLeft(ev, test(ev.getPlayer, ev.getBlock.getLocation))

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onHangingBreakByEntity(ev: HangingBreakByEntityEvent): Unit = {
    val loc = ev.getEntity.getLocation
    if (isPlotWorld(loc)) {
      ev.getRemover match {
        case p: Player => cancelLeft(ev, test(p, loc))
        case _ =>
      }
    }
  }

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onPlayerInteractEntity(ev: PlayerInteractEntityEvent): Unit = {
    val loc = ev.getRightClicked.getLocation
    if (isPlotWorld(loc)) {
      ev.getRightClicked.getType match {
        case MINECART | BOAT =>
        case _ => cancelLeft(ev, test(ev.getPlayer, loc))
      }
    }
  }

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onEntityDamageByEntity(ev: EntityDamageByEntityEvent): Unit = {
    val loc = ev.getEntity.getLocation
    if (isPlotWorld(loc) && !ev.getEntity.isInstanceOf[Player]) {
      ev.getDamager match {
        case p: Player => cancelLeft(ev, test(p, loc))
        case _ => ev.setCancelled(true)
      }
    }
  }

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onPlayerEggThrow(ev: PlayerEggThrowEvent): Unit = {
    if (test(ev.getPlayer, ev.getEgg.getLocation).isLeft)
      ev.setHatching(false)
  }

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onBlockBurn(ev: BlockBurnEvent): Unit =
    cancel(ev, isPlotWorld(ev.getBlock.getLocation))

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onVehicleDestroy(ev: VehicleDestroyEvent): Unit = {
    val loc = ev.getVehicle.getLocation
    if (isPlotWorld(loc)) {
      ev.getAttacker match {
        case p: Player => cancelLeft(ev, test(p, loc))
        case _ =>
      }
    }
  }

  @EventHandler(priority = LOWEST, ignoreCancelled = true)
  def onPlayerMove(ev: PlayerMoveEvent): Unit = {
    if (ev.getFrom.getBlockX != ev.getTo.getBlockX || ev.getFrom.getBlockZ != ev.getTo.getBlockZ) {
      inPlot(ev.getTo) { (w, plot) =>
        val p = ev.getPlayer
        plot.entryStatus(p) match {
          case Denied =>
              if (!p.hasPermission("plot.bypass.ban")) {
                p.teleport(p.getWorld.getSpawnLocation)
                p.sendMessage(err"You are banned from this plot")
              }
            case Closed =>
              if (!p.hasPermission("plot.bypass.close")) {
                p.teleport(p.getWorld.getSpawnLocation)
                p.sendMessage(err"This plot is closed to visitors")
              }
            case _ =>
          }
        }
      }
  }

  def isPastBorder(loc: Location): Boolean = {
    val border = loc.getWorld.getWorldBorder
    val size = border.getSize.toInt / 2
    val x = loc.getBlockX
    val z = loc.getBlockZ
    math.abs(x) > size || math.abs(z) > size
  }

  @EventHandler(priority = LOWEST, ignoreCancelled = true)
  def onTeleport(ev: PlayerTeleportEvent): Unit = {
    resolver(ev.getTo.getWorld) foreach { w =>
      val maybePlayer = Option(ev.getPlayer)
      if (isPastBorder(ev.getTo)) {
        maybePlayer.foreach(_.sendMessage(err"You cannot teleport past the world border"))
        ev.setCancelled(true)
      } else {
          maybePlayer foreach { p =>
            val id = w.getPlotId(ev.getTo)
            w.getPlot(id) foreach { plot =>
              if (!p.hasPermission("plot.bypass.ban") && plot.isBanned(p.getUniqueId)) {
                p.sendMessage(err"You cannot teleport to a plot that you are banned from")
                ev.setCancelled(true)
              } else if (!p.hasPermission("plot.bypass.close") && plot.closed && plot.status(p) < HelperOffline) {
                p.sendMessage(err"You cannot teleport to a plot that is closed to visitors")
                ev.setCancelled(true)
              }
            }
          }
      }
    }
  }

  @EventHandler
  def onItemDrop(ev: PlayerDropItemEvent): Unit = {
    val player = ev.getPlayer
    val spawnId = getPlotId(player.getWorld.getSpawnLocation)
    val id = getPlotId(player.getLocation)
    if (spawnId.isDefined && !player.hasPermission("plot.admin") && spawnId == id) {
      player.sendMessage(err"You cannot drop items at spawn")
      ev.getItemDrop.remove()
    }
  }

  @EventHandler(ignoreCancelled = true)
  def onCommandPreProcess(ev: PlayerCommandPreprocessEvent): Unit = {
    val player = ev.getPlayer
    val args = ev.getMessage.split(" ")
    val label = args(0).toLowerCase
    if (!player.hasPermission("plot.admin") && label.startsWith("/sethome")) {
      if (getPlot(player.getLocation)
          .map(plot => !plot.isAdded(player.getUniqueId))
          .getOrElse(true)) {
        player.sendMessage(err"You must be added to this plot to set home here")
        ev.setCancelled(true)
      }
    }
  }

  // TODO portal listener

  @EventHandler
  def onWorldLoad(ev: WorldLoadEvent): Unit = {
    val w = ev.getWorld
    if (isPlotWorld(w)) {
      w.setSpawnFlags(false, false)
      w.setAmbientSpawnLimit(0)
      w.setAnimalSpawnLimit(0)
      w.setMonsterSpawnLimit(0)
      w.setWaterAnimalSpawnLimit(0)
      w.setStorm(false)
      w.setPVP(true)
      w.setDifficulty(PEACEFUL)
      w.getWorldBorder.setCenter(0, 0)
      w.setGameRuleValue("mobGriefing", "false")
      w.setGameRuleValue("doMobSpawning", "false")
      w.setGameRuleValue("doFireTick", "false")
      w.setGameRuleValue("doMobLoot", "false")
      w.setGameRuleValue("showDeathMessages", "false")
      w.setGameRuleValue("doDaylightCycle", "true")
      w.setGameRuleValue("keepInventory", "true")
    }
  }


}