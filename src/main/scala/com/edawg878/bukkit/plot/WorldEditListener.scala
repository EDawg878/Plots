package com.edawg878.bukkit.plot

import com.edawg878.bukkit.plot.Plot._
import com.edawg878.common.Color.Formatter
import com.edawg878.common.Server.Server
import com.sk89q.worldedit.bukkit.WorldEditPlugin
import com.sk89q.worldedit.extension.platform.CommandManager
import com.sk89q.worldedit.function.mask.{Mask, RegionMask}
import com.sk89q.worldedit.regions.CuboidRegion
import com.sk89q.worldedit.regions.selector.CuboidRegionSelector
import com.sk89q.worldedit.{Vector => WorldEditVector}
import org.apache.commons.lang.StringUtils
import org.apache.commons.lang.math.NumberUtils
import org.bukkit.Location
import org.bukkit.entity.Player
import org.bukkit.event.player.PlayerCommandPreprocessEvent
import org.bukkit.event.{EventHandler, Listener}
import org.bukkit.plugin.Plugin

case class WorldEditConfig(maxBlockTypes: Int, bannedCommands: Set[String], selectionCommands: Set[String], limits: Map[String, Int]) {

  def isBanned(s: String): Boolean = bannedCommands.contains(s.toLowerCase)
  def isSelection(s: String): Boolean = selectionCommands.contains(s.toLowerCase)

}

case class WorldEditCommand(player: Player, message: String, label: String, split: Array[String], plot: Plot, world: PlotWorld)

class WorldEditListener(val resolver: PlotWorldResolver, val server: Server, plugin: Plugin, config: WorldEditConfig)
  extends Listener with PlotHelper {

  require(plugin.isInstanceOf[WorldEditPlugin], "plugin must be an instance of WorldEditPlugin")

  val worldedit: WorldEditPlugin = plugin.asInstanceOf[WorldEditPlugin]

  type FilterResult = Either[String, Unit]

  lazy val filters: Seq[WorldEditCommand => FilterResult] =
    Seq(banFilter, voteFilter, argsFilter, blockTypeFilter, radiusFilter, operationFilter)

  def banFilter(c: WorldEditCommand): FilterResult = {
    if (config.isBanned(c.label)) Left(err"This WorldEdit command is banned")
    else Right()
  }

  def voteFilter(c: WorldEditCommand): FilterResult = {
    if (hasVoted(c.player)) Right()
    else Left(info"You need to vote 3 times before unlocking WorldEdit")
  }

  def argsFilter(c: WorldEditCommand): FilterResult = {
    val session = worldedit.getSession(c.player)
    val limit = session.getBlockChangeLimit
    if (limit < 0 || c.split.flatMap(parseAbsDouble).forall(_ <= limit)) Right()
    else Left(err"You are not allowed to input numbers greater than $limit")
  }

  def blockTypeFilter(c: WorldEditCommand): FilterResult = {
    val limit = config.maxBlockTypes
    if (StringUtils.countMatches(c.message, ",") <= limit) Right()
    else
      Left(err"You may only use $limit block ${if (limit == 1) "type" else "types"} per operation")
  }

  def radiusFilter(c: WorldEditCommand): FilterResult = {
    config.limits.get(c.label).map(limit =>
      if (c.split.flatMap(parseAbsDouble).forall(_ > limit)) Left(err"You have exceeded the maximum radius of $limit")
      else Right()
    ) getOrElse Right()
  }

  def operationFilter(c: WorldEditCommand): FilterResult = {
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

  @EventHandler(ignoreCancelled = true)
  def onPlayerCommandPreprocess(ev: PlayerCommandPreprocessEvent): Unit = {
    val p = ev.getPlayer
    val args = commandManager.commandDetection(ev.getMessage.split(" "))
    if (!p.hasPermission("plot.admin") && isWorldEditCommand(args(0))) {
      val label = args(0).replace("/", "").toLowerCase
      withPlotStatus(p, Trusted, p => {
        p.sendMessage(err"You must be trusted to this plot in order to use WorldEdit here")
        ev.setCancelled(true)
      }) { (plotWorld, plot) =>
            val command = WorldEditCommand(p, ev.getMessage, label, args, plot, plotWorld)
            val cancel = filters.exists { f =>
              val res = f(command)
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
    val pos2 = new WorldEditVector(region.maxX, p.getWorld.getMaxHeight, region.maxZ)
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
