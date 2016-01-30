package com.edawg878.bukkit.plot

import com.edawg878.bukkit.plot.Plot._
import com.edawg878.common.PlotRepository
import com.edawg878.common.Server.Server
import com.sk89q.worldedit.bukkit.WorldEditPlugin
import com.sk89q.worldedit.extension.platform.CommandManager
import com.sk89q.worldedit.function.mask.{Mask, RegionMask}
import com.sk89q.worldedit.regions.CuboidRegion
import com.sk89q.worldedit.regions.selector.CuboidRegionSelector
import org.apache.commons.lang.StringUtils
import org.apache.commons.lang.math.NumberUtils
import org.bukkit.{Location, ChatColor}
import org.bukkit.entity.Player
import org.bukkit.event.{EventHandler, Listener}
import org.bukkit.event.player.PlayerCommandPreprocessEvent
import com.edawg878.common.Color.Formatter
import com.sk89q.worldedit.{Vector => WorldEditVector}

case class WorldEditConfig(maxBlockTypes: Int, bannedCommands: Set[String], selectionCommands: Set[String], limits: Map[String, Int]) {

  def isBanned(s: String): Boolean = bannedCommands.contains(s.toLowerCase)
  def isSelection(s: String): Boolean = selectionCommands.contains(s.toLowerCase)

}

case class WorldEditCommand(player: Player, message: String, label: String, split: Array[String], plot: Plot, world: PlotWorld)

object FilterResult {

  def check(b: Boolean, errorMessage: String): FilterResult = {
    if (b) success
    else error(errorMessage)
  }
  def error(s: String): FilterResult = FilterResult(false, Some(s))
  def success: FilterResult = FilterResult(true, None)

}
case class FilterResult(result: Boolean, message: Option[String])

trait WorldEditFilter {

  def check(c: WorldEditCommand): Either[String, Unit]

}

class WorldEditListener(val resolver: PlotWorldResolver, val server: Server, worldedit: WorldEditPlugin, config: WorldEditConfig)
  extends Listener with PlotHelper {

  lazy val filters: Seq[WorldEditFilter] = Seq(banFilter, voteFilter, argsFilter, blockTypeFilter, radiusFilter, operationFilter)

  val banFilter = new WorldEditFilter {
    override def check(c: WorldEditCommand): Either[String, Unit] = {
      if (config.isBanned(c.label)) Left(err"This WorldEdit command is banned")
      else Right()
    }
  }

  val voteFilter = new WorldEditFilter {
    override def check(c: WorldEditCommand): Either[String, Unit] = {
      if (hasVoted(c.player)) Right()
      else Left(info"You need to vote 3 times before unlocking WorldEdit")
    }
  }

  val argsFilter = new WorldEditFilter {

    override def check(c: WorldEditCommand): Either[String, Unit] = {
      val session = worldedit.getSession(c.player)
      val limit = session.getBlockChangeLimit
      if (limit < 0 || c.split.flatMap(parseAbsDouble).forall(_ <= limit)) Right()
      else Left(err"You are not allowed to input numbers greater than $limit")
    }
  }

  val blockTypeFilter = new WorldEditFilter {

    override def check(c: WorldEditCommand): Either[String, Unit] = {
      val limit = config.maxBlockTypes
      if (StringUtils.countMatches(c.message, ",") <= limit ||
        c.player.hasPermission("plot.admin")) Right()
      else
        Left(err"You may only use $limit block ${if (limit == 1) "type" else "types"} per operation")
    }

  }

  val radiusFilter = new WorldEditFilter {

    override def check(c: WorldEditCommand): Either[String, Unit] =
      config.limits.get(c.label).map(limit =>
        if (c.split.flatMap(parseAbsDouble).forall(_ > limit)) Left(err"You have exceeded the maximum radius of $limit")
        else Right()
      ) getOrElse(Right())

  }

  val operationFilter = new WorldEditFilter {

    override def check(c: WorldEditCommand): Either[String, Unit] = {
      Option(worldedit.getSelection(c.player)).map { selection =>
        val selector = selection.getRegionSelector
        if (selector.isInstanceOf[CuboidRegionSelector]) {
          if (!selector.isDefined) Right()
          else {
            val id = c.plot.id
            val plotConfig = c.world.config
            val roadAccess = c.plot.roadAccess
            val min = selection.getMinimumPoint
            val max = selection.getMaximumPoint
            def insidePlot(loc: Location) = {
              val r = if (roadAccess) plotConfig.outer(id) else plotConfig.inner(id)
              r.isInside(loc)
            }
            if (insidePlot(min) && insidePlot(max)) {
              val session = worldedit.getSession(c.player)
              val limit = session.getBlockChangeLimit
              if (limit < 0 || selection.getArea <= limit) Right()
              else Left(err"You have exceeded the maximum radius $limit for this command")
            } else {
              selector.clear()
              Left(err"Your WorldEdit selection extended outside of the plot")
            }
          }
        } else {
          selector.clear()
          Left(err"Only cuboid regions are supported")
        }
      } getOrElse(Right())
    }

  }

  @EventHandler
  def onPlayerCommandPreprocess(ev: PlayerCommandPreprocessEvent): Unit = {
    val p = ev.getPlayer
    val args = commandManager.commandDetection(ev.getMessage.split(" "))
    if (isWorldEditCommand(args(0))) {
      val label = args(0).replace("/", "").toLowerCase
      withPlotStatus(p, Trusted, p => {
        p.sendMessage(err"You must be trusted to this plot in order to use WorldEdit here")
        ev.setCancelled(true)
      }) { (plotWorld, plot) =>
            val command = WorldEditCommand(p, ev.getMessage, label, args, plot, plotWorld)
            val cancel = filters.exists { f =>
              val res = f.check(command)
              res.left.foreach(p.sendMessage)
              res.isLeft
            }
            if (cancel) ev.setCancelled(true)
            else setMask(p, plot, plotWorld)
        }
    }
  }

  def setMask(p: Player, plot: Plot, world: PlotWorld): Unit = {
    val region = if (plot.roadAccess) world.config.outer(plot.id) else world.config.inner(plot.id)
    val session = worldedit.getSession(p)
    val pos1 = new WorldEditVector(region.minX, world.config.MinY, region.minZ)
    val pos2 = new WorldEditVector(region.maxX, world.config.MaxY, region.maxZ)
    val cuboid = new CuboidRegion(session.getSelectionWorld, pos1, pos2)
    val mask = new RegionMask(cuboid)
    session.setMask(mask)
  }

  def removeMask(p: Player): Unit = {
    val session = worldedit.getSession(p)
    val mask: Mask = null
    session.setMask(mask)
  }

  def commandManager: CommandManager = worldedit.getWorldEdit.getPlatformManager.getCommandManager

  def isWorldEditCommand(base: String): Boolean = {
    commandManager.getDispatcher.contains(base)
  }

  def hasVoted(p: Player): Boolean = {
    p.hasPermission("vote") || p.hasPermission("perk.worldedit")
  }

  def parseAbsDouble(s: String): Option[Double] = {
    if (NumberUtils.isNumber(s)) Some(math.abs(NumberUtils.toDouble(s)))
    else None
  }

}
