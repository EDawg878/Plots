package com.edawg878.bukkit.listener

import com.edawg878.bukkit.plot.Plot._
import com.edawg878.bukkit.plot._
import com.edawg878.common.PlotRepository
import com.edawg878.common.Server.Server
import org.bukkit.block.Block
import org.bukkit.event.entity.{EntityDamageByEntityEvent, EntityExplodeEvent, EntityChangeBlockEvent}
import org.bukkit.event.hanging.{HangingBreakByEntityEvent, HangingPlaceEvent}
import org.bukkit.event.player._
import org.bukkit.event.vehicle.VehicleDestroyEvent
import org.bukkit.event.world.{WorldLoadEvent, StructureGrowEvent}
import org.bukkit._
import org.bukkit.entity._
import org.bukkit.event.block._
import org.bukkit.entity.EntityType._
import com.edawg878.common.Color.Formatter
import org.bukkit.event._
import org.bukkit.event.EventPriority._
import org.bukkit.event.block.Action._
import org.bukkit.Difficulty._
import scala.collection.JavaConverters._
import scala.collection.JavaConversions._
import com.edawg878.bukkit.plot.PlotHelper._

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

  def cancel(r: Result, f: => Unit): Unit = if (r.isLeft) f
  def cancel(ev: Cancellable, r: Result): Unit = cancel(r, ev.setCancelled(true))
  def cancel(ev: Cancellable, cond: Boolean): Unit = if (cond) ev.setCancelled(true)

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
    cancel(ev, resetExpiration(ev.getPlayer, ev.getBlock.getLocation))

  @EventHandler
  def onBlockPlace(ev: BlockPlaceEvent): Unit =
    cancel(ev, resetExpiration(ev.getPlayer, ev.getBlock.getLocation))

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onEntityChangeBlock(ev: EntityChangeBlockEvent): Unit = {
    val loc = ev.getBlock.getLocation
    if (isPlotWorld(loc)) {
      ev.getEntity match {
        case p: Player =>
          cancel(ev, test(p, loc))
        case e if !e.isInstanceOf[FallingBlock] =>
          ev.setCancelled(true)
        case _ =>
      }
    }
  }

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onEntityBlockForm(ev: EntityBlockFormEvent): Unit = {
    if (isPlotWorld(ev.getBlock.getLocation) && !ev.getEntity.isInstanceOf[Player])
      ev.setCancelled(true)
  }

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onPlayerBucketEmpty(ev: PlayerBucketEmptyEvent): Unit = {
    val face = ev.getBlockFace
    val loc = ev.getBlockClicked.getLocation
      .add(face.getModX, face.getModY, face.getModZ)
      .getBlock
      .getLocation
    cancel(ev, test(ev.getPlayer, loc, Trusted, err"You must be trusted to the plot in order to empty buckets"))
  }

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onPlayerBucketFill(ev: PlayerBucketFillEvent): Unit =
    cancel(ev, test(ev.getPlayer, ev.getBlockClicked.getLocation, Trusted, err"You must be trusted to the plot in order to fill buckets"))

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onPlayerInteract(ev: PlayerInteractEvent): Unit = {
    val p = ev.getPlayer
    if (isPlotWorld(p.getWorld) && ev.getAction == RIGHT_CLICK_BLOCK) {
      Option(ev.getClickedBlock).map(_.getLocation).foreach { loc =>

        /*
            if (!settings.isAllowedInteract(block.getType.name)) {
            checkCancel(event, player, block.getLocation)
            }
         */

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
  def onStructureGrow(ev: StructureGrowEvent): Unit = {
    val locA = ev.getLocation
    getPlot(locA).foreach(plot =>
      ev.getBlocks.removeAll(
        ev.getBlocks.asScala.filter(b => b != null && isInterplot(plot, locA, b.getLocation))))
  }

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onEntityExplode(ev: EntityExplodeEvent): Unit =
    Option(ev.getLocation).foreach(loc => cancel(ev, isPlotWorld(loc)))

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onBlockIgnite(ev: BlockIgniteEvent): Unit =
    Option(ev.getPlayer).fold(ev.setCancelled(true))(p =>
      cancel(ev, test(p, ev.getBlock.getLocation, Trusted, err"You must be trusted to the plot in order to ignite blocks")))

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onHangingPlace(ev: HangingPlaceEvent): Unit =
    cancel(ev, test(ev.getPlayer, ev.getBlock.getLocation))

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onHangingBreakByEntity(ev: HangingBreakByEntityEvent): Unit = {
    val loc = ev.getEntity.getLocation
    if (isPlotWorld(loc)) {
      ev.getRemover match {
        case p: Player => cancel(ev, test(p, loc))
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
        case _ => cancel(ev, test(ev.getPlayer, loc))
      }
    }
  }

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onEntityDamageByEntity(ev: EntityDamageByEntityEvent): Unit = {
    val loc = ev.getEntity.getLocation
    if (isPlotWorld(loc) && !ev.getEntity.isInstanceOf[Player]) {
      ev.getDamager match {
        case p: Player => cancel(ev, test(p, loc))
        case _ => ev.setCancelled(true)
      }
    }
  }

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onPlayerEggThrow(ev: PlayerEggThrowEvent): Unit =
    cancel(test(ev.getPlayer, ev.getEgg.getLocation), ev.setHatching(false))

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onBlockBurn(ev: BlockBurnEvent): Unit =
    cancel(ev, isPlotWorld(ev.getBlock.getLocation))

  @EventHandler(priority = HIGH, ignoreCancelled = true)
  def onVehicleDestroy(ev: VehicleDestroyEvent): Unit = {
    val loc = ev.getVehicle.getLocation
    if (isPlotWorld(loc)) {
      ev.getAttacker match {
        case p: Player => cancel(ev, test(p, loc))
        case _ =>
      }
    }
  }

  @EventHandler(priority = LOWEST, ignoreCancelled = true)
  def onPlayerMove(ev: PlayerMoveEvent): Unit = {
    if (ev.getFrom.getBlockX != ev.getTo.getBlockX
      || ev.getFrom.getBlockZ != ev.getTo.getBlockZ) {
      resolver(ev.getTo.getWorld) foreach { pm =>
        val p = ev.getPlayer
        if (pm.border.isPast(ev.getTo)) {
          ev.setTo(pm.border.knockback(ev.getTo))
          p.sendMessage(err"You have reached the end of this world")
        } else {
          val id = pm.getPlotId(ev.getTo)
          pm.getPlot(id) foreach { plot =>
            plot.entryStatus(p) match {
              case Denied =>
                // TODO knockback
                p.sendMessage(err"You are banned from this plot")
              case Closed =>
                // TODO knockback
                p.sendMessage(err"This plot is closed to visitors")
              case _ =>
            }
          }
        }
      }
    }
  }

  @EventHandler(priority = LOWEST, ignoreCancelled = true)
  def onTeleport(ev: PlayerTeleportEvent): Unit = {
    resolver(ev.getTo.getWorld) foreach { pm =>
      Option(ev.getPlayer) foreach { p =>
        val id = pm.getPlotId(ev.getTo)
        pm.getPlot(id) foreach { plot =>
          if (!plot.status(p).has(NotBanned)) {
            if (plot.isBanned(p.getUniqueId)) {
              ev.setCancelled(true)
              p.sendMessage(err"You cannot teleport to a plot that you are banned from")
            } else if (plot.closed && !plot.isAdded(p.getUniqueId)) {
              ev.setCancelled(true)
              p.sendMessage(err"You cannot teleport to a plot that is closed to visitors")
            }
          }
        }
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