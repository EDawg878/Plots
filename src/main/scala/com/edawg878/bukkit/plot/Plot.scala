package com.edawg878.bukkit.plot

import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalAmount
import java.time.{Duration, Instant, LocalDate}
import java.util.{Random, UUID}

import com.edawg878.bukkit.plot.Plot._
import com.edawg878.common.PlotRepository
import com.edawg878.common.Server.Server
import org.bukkit.Material._
import org.bukkit.block.Biome
import org.bukkit.block.Biome._
import org.bukkit.entity.Player
import org.bukkit.generator.ChunkGenerator
import org.bukkit.generator.ChunkGenerator.BiomeGrid
import org.bukkit.{ChatColor, Location, Material, World}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Try
import scala.collection.mutable
import com.edawg878.bukkit.BukkitConversions._
import com.edawg878.common.Color.Formatter

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
case class PlotStyle(border: Material = DOUBLE_STEP,
                     bottom: Material = BEDROCK,
                     top: Material = GRASS,
                     filling: Material = DIRT,
                     wall: Material = DOUBLE_STEP,
                     path: Material = WOOD,
                     biome: Biome = PLAINS)

case class PlotWorld(name: String,
                     plotSize: Int = 64,
                     borderHeight: Int = 65,
                     topHeight: Int = 64,
                     maxHeight: Int = 256,
                     pathHeight: Int = 65,
                     pathWidth: Int = 3,
                     buildOnFloor: Boolean = false,
                     style: PlotStyle = PlotStyle()) {

  val ChunkSize = 16
  val ChunkExp = Math.getExponent(ChunkSize)
  val WorldExp = Math.getExponent(plotSize)

  private def isBorder(x: Int, z: Int, border: Int => Boolean): Boolean = {
    val px = x & (plotSize - 1)
    val pz = z & (plotSize - 1)
    border(px) || border(pz)
  }

  private def isTouchingSide(a: Int, b: Int): Boolean =
    a == b || a == plotSize - b + 1

  private def isBorder(x: Int, z: Int, start: Int, end: Int): Boolean =
    isBorder(x, z, a => (start until end).exists(isTouchingSide(a, _)))

  private def isBorder(x: Int, z: Int, dist: Int): Boolean =
    isBorder(x, z, isTouchingSide(_, dist))

  def isBorder(x: Int, z: Int): Boolean =
    isBorder(x, z, dist = pathWidth)

  def isPath(x: Int, z: Int): Boolean =
    isBorder(x, z, start = 0, end = pathWidth)

  def isBorder(loc: Location): Boolean = isBorder(loc.getBlockX, loc.getBlockZ)

  def isPath(loc: Location): Boolean = isPath(loc.getBlockX, loc.getBlockZ)

}

case class Position(x: Int, y: Int, z: Int)

case class Corners(bottom: Position, top: Position)

case class Region(minX: Int, maxX: Int, minZ: Int, maxZ: Int)

object ChunkRegion {

  def apply(w: PlotWorld, id: PlotId): ChunkRegion = {
    val exp = w.WorldExp - w.ChunkExp
    val chunks = (w.plotSize >> 4) - 1
    val minX = id.x << exp
    val minZ = id.z << exp
    val maxX = minX + chunks
    val maxZ = minZ + chunks
    ChunkRegion(minX, maxX, minZ, maxZ)
  }

}

case class ChunkRegion(minX: Int, maxX: Int, minZ: Int, maxZ: Int)

object PlotId {

  def parse(w: PlotWorld, input: String): Option[PlotId] = {
    val s = input.split(";")
    Try(PlotId(s(0).toInt, s(1).toInt, w.name)).toOption
  }

  def of(w: PlotWorld, x: Int, z: Int) = this(x >> w.WorldExp, z >> w.WorldExp, w.name)

}

case class PlotId(x: Int, z: Int, world: String) {

  def border(w: PlotWorld): Border = {
    val r = region(w)
    Border(buffer = 0, knockback = 5, minX = r.minX, maxX = r.maxX, minZ = r.minZ, maxZ = r.maxZ)
  }

  def corners(w: PlotWorld): Corners = {
    val blockX: Int = x << w.WorldExp
    val blockZ: Int = z << w.WorldExp
    val bottom = Position(blockX, 0, blockZ)
    val top = Position(blockX + w.plotSize - 1, w.maxHeight, blockZ + w.plotSize - 1)
    Corners(bottom, top)
  }

  def region(w: PlotWorld): Region = {
    val blockX: Int = x << w.WorldExp
    val blockZ: Int = z << w.WorldExp
    val size = w.plotSize - 1
    Region(minX = blockX, maxX = blockX + size, minZ = blockZ, maxZ = blockZ + size)
  }

  def isInside(w: PlotWorld, l: Location): Boolean =
    w.name == l.getWorld.getName && isInside(w, l.toPosition)

  def isInside(w: PlotWorld, p: Position): Boolean = {
    val c = corners(w)
    p.x >= c.bottom.x &&
      p.x <= c.top.x &&
      p.y >= c.bottom.y &&
      p.y <= c.top.y &&
      p.z >= c.bottom.z &&
      p.z <= c.top.z
  }

  def distanceSq(id: PlotId): Double = math.pow(id.x - x, 2) + math.pow(id.z - z, 2)

  def distance(id: PlotId): Double = math.sqrt(distance(id))

  override def toString: String = Seq(x, z).mkString(";")

}

object Plot {

  sealed trait Status {
    def rank: Int
    def has(that: Status): Boolean = this.rank >= that.rank
  }

  case object Admin extends Status { val rank = 100 }
  case object Owner extends Status { val rank = 20 }
  case object Trusted extends Status { val rank = 5 }
  case object HelperOnline extends Status { val rank = 3 }
  case object HelperOffline extends Status { val rank = 2 }
  case object NotBanned extends Status { val rank = 1 }
  case object Banned extends Status { val rank = 0 }

  sealed trait EntryStatus
  case object Allowed extends EntryStatus
  case object Denied extends EntryStatus
  case object Closed extends EntryStatus

  object Result {
    def apply(b: Boolean) = if (b) True else False
  }

  sealed trait Result
  case object Error extends Result
  case object True extends Result
  case object False extends Result
  case object Invalid extends Result

  def newExpiration: LocalDate = LocalDate.now.plusDays(30)

  def apply(owner: UUID, id: PlotId): Plot = new Plot(
    id = id,
    owner = owner,
    timeClaimed = Instant.now,
    expirationDate = newExpiration
  )

}

case class Plot(id: PlotId,
                owner: UUID,
                alias: Option[String] = None,
                timeClaimed: Instant,
                expirationDate: LocalDate,
                protect: Boolean = false,
                closed: Boolean = false,
                roadAccess: Boolean = false,
                lastCleared: Option[Instant] = None,
                helpers: Set[UUID] = Set(),
                trusted: Set[UUID] = Set(),
                banned: Set[UUID] = Set())
  extends Ordered[Plot] {

  val ExpirationFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("MM/dd/yy")

  def resetExpire: Option[Plot] = {
    val updated = Plot.newExpiration
    if (updated == expirationDate) None
    else Some(copy(expirationDate = updated))
  }

  def open: Boolean = !closed

  def isExpired: Boolean = !protect && LocalDate.now.isAfter(expirationDate)

  def clearAdded: Plot = copy(helpers = Set(), trusted = Set())

  def clearBanned: Plot = copy(banned = Set())

  def toggleRoadAccess: Plot = copy(roadAccess = !roadAccess)

  def ids: Set[UUID] = helpers ++ trusted ++ banned + owner

  def canClear(amt: TemporalAmount): Boolean =
    lastCleared.fold(true)(t => Instant.now().isAfter(t.plus(amt)))

  def status(p: Player): Status = {
    if (p.hasPermission("plot.admin")) Admin
    else if (isOwner(p)) Owner
    else if (isTrusted(p)) Trusted
    else if (isHelper(p)) HelperOffline
    else if (isBanned(p)) Banned
    else NotBanned
  }

  def status(s: Server, p: Player): Status = {
    val st = status(p)
    if (st == HelperOffline && s.isOnline(p.getUniqueId)) HelperOnline
    else st
  }

  def entryStatus(p: Player): EntryStatus = {
    val pid = p.getUniqueId
    if (banned contains pid) Denied
    else if (closed && !status(p).has(NotBanned)) Closed
    else Allowed
  }

  def isOwner(pid: UUID): Boolean = owner == pid
  def isHelper(pid: UUID): Boolean = helpers.contains(pid)
  def isTrusted(pid: UUID): Boolean = trusted.contains(pid)
  def isAdded(pid: UUID): Boolean = isHelper(pid) || isTrusted(pid)
  def isBanned(pid: UUID): Boolean = banned.contains(pid)

  def isOwner(p: Player): Boolean = isOwner(p.getUniqueId)
  def isHelper(p: Player): Boolean = isHelper(p.getUniqueId)
  def isTrusted(p: Player): Boolean = isTrusted(p.getUniqueId)
  def isAdded(p: Player): Boolean = isAdded(p.getUniqueId)
  def isBanned(p: Player): Boolean = isBanned(p.getUniqueId)

  override def compare(that: Plot): Int = this.timeClaimed compareTo that.timeClaimed
}

case class Border(buffer: Int,
                  knockback: Int,
                  maxX: Int = 0,
                  minX: Int = 0,
                  maxZ: Int = 0,
                  minZ: Int = 0) {

  def recalculate(w: PlotWorld, id: PlotId): Border = {
    val r = id.region(w)
    copy(minX = minX.min(r.minX) - buffer,
      maxX = maxX.max(r.maxX) + buffer,
      minZ = minZ.min(r.minZ) - buffer,
      maxZ = maxZ.max(r.maxZ) + buffer
    )
  }

  def isPast(loc: Location): Boolean = {
    val locX = loc.getBlockX
    val locZ = loc.getBlockZ
    locX >= maxX ||
      locX <= minX ||
      locZ >= maxZ ||
      locZ <= minZ
  }

  def knockback(loc: Location): Location = {
    val locX = loc.getX
    val locZ = loc.getZ
    val x =
      if (locX <= minX) minX + knockback
      else if (locX >= maxX) maxX - knockback
      else locX
    val z =
      if (locZ <= minZ) minZ + knockback
      else if (locZ >= maxZ) maxZ - knockback
      else locZ
    val adjusted = loc.clone
    adjusted.setX(x)
    adjusted.setZ(z)
    adjusted
  }

}

case class PlotWorldConfig(worlds: Seq[PlotWorld])

//case class Config(message: String)

object PlotManager {

  def load(w: PlotWorld, db: PlotRepository): Future[PlotManager] = {
    Future {
      val res = mutable.Map[PlotId, Plot]()

      for (pSeq <- db.findAll(w)) {
        for (p <- pSeq) {
          res.put(p.id, p)
        }
      }

      PlotManager(w, res, findBorder(w, res))
    }
  }

  def findBorder(w: PlotWorld, plots: mutable.Map[PlotId, Plot]): Border = {
    val r = plots.map {
      case (id, p) => ChunkRegion(w, id)
    }
    val buffer = w.plotSize * 3
    val knockback = w.pathWidth - 1
    if (r.isEmpty) {
      Border(buffer, knockback, minX = -buffer, maxX = buffer, minZ = -buffer, maxZ = buffer)
    } else {
      val _minX = r.map(_.minX).min
      val _maxX = r.map(_.maxX).max
      val _minZ = r.map(_.minZ).min
      val _maxZ = r.map(_.maxZ).max
      Border(buffer, knockback, minX = _minX - buffer, maxX = _maxX + buffer, minZ = _minZ - buffer, maxZ = _maxZ + buffer)
    }
  }

}

case class PlotManager(w: PlotWorld, plots: mutable.Map[PlotId, Plot], var border: Border) {

  def update(plot: Plot): Unit = plots.put(plot.id, plot)

  def getPlotId(loc: Location): PlotId = PlotId.of(w, loc.getBlockX, loc.getBlockZ)

  def getPlot(id: PlotId): Option[Plot] = plots.get(id)

  def claim(p: Player, id: PlotId): Plot = {
    val plot = Plot(p.getUniqueId, id)
    plots.put(id, plot)
    border = border.recalculate(w, id)
    plot
  }

  def unclaim(id: PlotId): Option[Plot] = plots.remove(id)

  def getHomeLocation(bw: World, id: PlotId): Location = {
    val c = id.corners(w).bottom
    val x = c.x + w.pathWidth
    val z = c.z + w.pathWidth
    val y = bw.getHighestBlockYAt(x, z)
    val yaw = -45f
    val pitch = 0f
    new Location(bw, x, y, z, yaw, pitch)
  }

  def getHomes(p: Player): Seq[Plot] = getHomes(p.getUniqueId)

  def getHomes(id: UUID): Seq[Plot] = plots.values.filter(_.isOwner(id)).toSeq.sorted

  def getHome(id: UUID, n: Int): Option[Plot] = Try(getHomes(id)(n - 1)).toOption

  def getHome(p: Player, n: Int): Option[Plot] = getHome(p.getUniqueId, n)

  def getHome(id: UUID, s: String): Option[Plot] = getHomes(id).find(_.alias.contains(s))

  def getHome(p: Player, s: String): Option[Plot] = getHome(p.getUniqueId, s)

  def isInside(l: Location, id: PlotId): Boolean =
    l.getWorld.getName == w.name && id.isInside(w, l.toPosition)

  def isClaimed(id: PlotId): Boolean = plots.contains(id)

  def clear(id: PlotId): Unit = {}

}

trait PlotHelper {

  def pms: World => Option[PlotManager]
  def server: Server

  def isPlotWorld(w: World): Boolean = pms(w).isDefined

  def isPlotWorld(loc: Location): Boolean = isPlotWorld(loc.getWorld)

  def inPlotWorld(p: Player)(pm: PlotManager => Unit): Unit =
    pms(p.getWorld).map(pm).getOrElse(p.sendMessage(err"You must be in a plot world to execute this command"))

  def withPlotStatus(p: Player, st: Status, err: Player => Unit)(f: (PlotManager, Plot) => Unit): Unit =
    inPlotWorld(p) { pm =>
      pm.getPlot(pm.getPlotId(p.getLocation)).fold(p.sendMessage(err"No plot found")) { plot =>
        if (plot.status(server, p).has(st)) f(pm, plot)
        else err(p)
      }
    }

 def getPlot(loc: Location): Option[Plot] =
   pms(loc.getWorld).flatMap(pm => pm.getPlot(pm.getPlotId(loc)))

  private def failure(p: Player, s: String*): Result = { s.foreach(p sendMessage); Error }

  def has(st: Status, p: Player, loc: Location, s: Server): Result = {
    pms(loc.getWorld).fold[Result](Invalid) { pm =>
      if (!pm.w.buildOnFloor && loc.getBlockY <= 0) failure(p, err"You cannot build on the floor level")
      else {
        if (p hasPermission "plot.admin") True
        else pm.getPlot(pm.getPlotId(loc))
            .fold(failure(p, err"You cannot build here")){ plot =>
          if (!plot.roadAccess && (pm.w.isBorder(loc) || pm.w.isPath(loc))) {
            failure(p, err"You cannot build here")
          } else {
            st match {
              case HelperOnline =>
                if (plot.isOwner(p) || plot.isTrusted(p)) True
                else if (plot.isHelper(p)) {
                  if (s.isOnline(plot.owner)) True
                  else {
                    p.sendMessage(err"You cannot modify the plot while its owner is offline unless you are trusted")
                    Error
                  }
                } else {
                  False
                }
              case HelperOffline =>
                Result(plot.isOwner(p) || plot.isHelper(p) || plot.isTrusted(p))
              case Owner =>
                Result(plot.isOwner(p))
              case Trusted =>
                Result(plot.isOwner(p) || plot.isTrusted(p))
              case Banned =>
                Result(plot.isBanned(p))
              case NotBanned =>
                Result(!plot.isBanned(p))
              case _ =>
                False
            }
          }}
      }
    }
  }

}

class PlotGenerator(w: PlotWorld, maxY: Int) extends ChunkGenerator {

  def this(w: PlotWorld) = this(w, math.max(w.pathHeight, math.max(w.borderHeight, w.topHeight)))

  override def generateExtBlockSections(world: World, random: Random, cx: Int, cz: Int, biomes: BiomeGrid): Array[Array[Short]] = {

    val result = new Array[Array[Short]](world.getMaxHeight >> 4)

    for {
      x <- 0 until 16
      locX = (cx << 4) + x
      z <- 0 until 16
      locZ = (cz << 4) + z
    } yield setBlocks(x, z, locX, locZ)

    def setBlocks(x: Int, z: Int, locX: Int, locZ: Int): Unit = {
      Option(biomes).foreach(_.setBiome(x, z, w.style.biome))

      val path = w.isPath(locX, locZ)
      val border = w.isBorder(locX, locZ)

      if (path) {
        setBlock(result, x, w.pathHeight, z, w.style.path)
      } else if (border) {
        setBlock(result, x, w.borderHeight, z, w.style.border)
      } else {
        setBlock(result, x, w.topHeight, z, w.style.top)
      }

      setBlock(result, x, 0, z, w.style.bottom)

      for (y <- 1 until maxY) {
        if (border && !path && y < w.borderHeight) {
          setBlock(result, x, y, z, w.style.wall)
        } else if (path || y < w.topHeight) {
          setBlock(result, x, y, z, w.style.filling)
        }
      }
    }

    result
  }

  def setBlock(result: Array[Array[Short]], x: Int, y: Int, z: Int, m: Material): Unit = {
    if (result(y >> 4) == null) {
      result(y >> 4) = new Array[Short](4096)
    }
    result(y >> 4)(((y & 0xF) << 8) | (z << 4) | x) = m.getId.toShort
  }

}