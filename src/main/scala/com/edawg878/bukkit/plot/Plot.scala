package com.edawg878.bukkit.plot

import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalAmount
import java.time.{Duration, Instant, LocalDate}
import java.util.concurrent.ConcurrentHashMap
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
import org.bukkit._

import scala.collection.concurrent.TrieMap
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

case class PlotWorldConfig(name: String,
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
  val MaxY = math.max(pathHeight, math.max(borderHeight, topHeight))

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

object PlotId {

  def parse(c: PlotWorldConfig, p: String): Option[PlotId] = {
    val s = p.split(";")
    Try(PlotId(s(0).toInt, s(1).toInt, c.name)).toOption
  }

  def of(c: PlotWorldConfig, x: Int, z: Int) = this(x >> c.WorldExp, z >> c.WorldExp, c.name)

}

case class PlotId(x: Int, z: Int, world: String) {

  def border(c: PlotWorldConfig): Border = {
    val blockX = x << c.WorldExp
    val blockZ = z << c.WorldExp
    Border(buffer = 0, knockback = -5,
      minX = blockX, maxX = blockX + c.plotSize,
      minZ = blockZ, maxZ = blockZ + c.plotSize)
  }

  def corners(c: PlotWorldConfig): Corners = {
    val blockX = x << c.WorldExp
    val blockZ = z << c.WorldExp
    val bottom = Position(blockX, 0, blockZ)
    val top = Position(blockX + c.plotSize - 1, c.maxHeight, blockZ + c.plotSize - 1)
    Corners(bottom, top)
  }

  def region(c: PlotWorldConfig): Region = {
    val blockX = x << c.WorldExp
    val blockZ = z << c.WorldExp
    val size = c.plotSize
    val minX = blockX
    val maxX = blockX + size
    val minZ = blockZ
    val maxZ = blockZ + size
    Region(minX, maxX, minZ, maxZ)
  }

  def chunkRegion(c: PlotWorldConfig): Region = {
    val exp = c.WorldExp - c.ChunkExp
    val chunks = (c.plotSize >> 4) - 1
    val minX = x << exp
    val minZ = z << exp
    val maxX = minX + chunks
    val maxZ = minZ + chunks
    Region(minX, maxX, minZ, maxZ)
  }

  def isInside(c: PlotWorldConfig, l: Location): Boolean =
    c.name == l.getWorld.getName && isInside(c, l.toPosition)

  def isInside(c: PlotWorldConfig, p: Position): Boolean = {
    val cs = corners(c)
    p.x >= cs.bottom.x &&
      p.x <= cs.top.x &&
      p.y >= cs.bottom.y &&
      p.y <= cs.top.y &&
      p.z >= cs.bottom.z &&
      p.z <= cs.top.z
  }

  def distanceSq(id: PlotId): Double = math.pow(id.x - x, 2) + math.pow(id.z - z, 2)

  def distance(id: PlotId): Double = math.sqrt(distanceSq(id))

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
    lastCleared.fold(true)(t => Instant.now.isAfter(t.plus(amt)))

  def status(p: Player): Status = {
    val pid = p.getUniqueId
    if (p.hasPermission("plot.admin")) Admin
    else if (isOwner(pid)) Owner
    else if (isTrusted(pid)) Trusted
    else if (isHelper(pid)) HelperOffline
    else if (isBanned(pid)) Banned
    else NotBanned
  }

  def status(s: Server, p: Player): Status = {
    val st = status(p)
    if (st == HelperOffline && s.isOnline(p.getUniqueId)) HelperOnline
    else st
  }

  def entryStatus(p: Player): EntryStatus = {
    if (banned contains p.getUniqueId) Denied
    else if (closed && !status(p).has(NotBanned)) Closed
    else Allowed
  }

  def isOwner(pid: UUID): Boolean = owner == pid
  def isHelper(pid: UUID): Boolean = helpers.contains(pid)
  def isTrusted(pid: UUID): Boolean = trusted.contains(pid)
  def isAdded(pid: UUID): Boolean = isHelper(pid) || isTrusted(pid)
  def isBanned(pid: UUID): Boolean = banned.contains(pid)

  override def compare(that: Plot): Int = this.timeClaimed compareTo that.timeClaimed
}

case class Border(buffer: Int,
                  knockback: Int,
                  minX: Int = 0,
                  maxX: Int = 0,
                  minZ: Int = 0,
                  maxZ: Int = 0) {
  
  def recalculate(r: Region): Border =
    copy(minX = minX.min(r.minX) - buffer,
      maxX = maxX.max(r.maxX) + buffer,
      minZ = minZ.min(r.minZ) - buffer,
      maxZ = maxZ.max(r.maxZ) + buffer)

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

trait PlotWorldResolver {

  def apply(s: String): Option[PlotWorld]

  def apply(bw: World): Option[PlotWorld] = apply(bw.getName)

}

object PlotWorld {

  def load(c: PlotWorldConfig, db: PlotRepository): Future[PlotWorld] =
    db.findAll(c.name)
        .map(_.map(p => p.id -> p))
        .map(TrieMap.apply)
        .map(plots => PlotWorld(c, plots, findBorder(c, plots.keys)))

  def findBorder(c: PlotWorldConfig, ids: Iterable[PlotId]): Border = {
    val regions = ids.map(_.chunkRegion(c))
    val buffer = c.plotSize
    val knockback = c.pathWidth - 1
    val start = Border(buffer, knockback, -buffer, buffer, -buffer, buffer)
    if (regions.isEmpty) start
    else {
      val region = regions.reduce((a, b) =>
        Region(math.min(a.minX, b.minX), math.max(a.maxX, b.maxX), math.min(a.minZ, b.minZ), math.max(a.maxZ, b.maxZ)))
      start.recalculate(region)
    }
  }

}

case class PlotWorld(config: PlotWorldConfig, plots: TrieMap[PlotId, Plot], var border: Border) {

  def update(plot: Plot): Unit = plots.put(plot.id, plot)

  def getPlotId(loc: Location): PlotId = PlotId.of(config, loc.getBlockX, loc.getBlockZ)

  def getPlot(id: PlotId): Option[Plot] = plots.get(id)

  def claim(p: Player, id: PlotId): Plot = {
    val plot = Plot(p.getUniqueId, id)
    plots.put(id, plot)
    plot
  }

  def unclaim(id: PlotId): Option[Plot] = plots.remove(id)

  def getHomeLocation(bw: World, id: PlotId): Location = {
    val c = id.corners(config).bottom
    val x = c.x + config.pathWidth
    val z = c.z + config.pathWidth
    val y = bw.getHighestBlockYAt(x, z)
    val yaw = -45f
    val pitch = 0f
    new Location(bw, x, y, z, yaw, pitch)
  }
  
  def getHomes(pid: UUID): Seq[Plot] = plots.values.filter(_.isOwner(pid)).toSeq.sorted

  def getHome(pid: UUID, n: Int): Option[Plot] = Try(getHomes(pid)(n - 1)).toOption
  
  def getHome(pid: UUID, s: String): Option[Plot] = getHomes(pid).find(_.alias.contains(s))
  
  def isInside(l: Location, id: PlotId): Boolean =
    l.getWorld.getName == config.name && id.isInside(config, l.toPosition)

  def isClaimed(id: PlotId): Boolean = plots.contains(id)

  def clear(bw: World, id: PlotId): Unit = {
    require(Bukkit.isPrimaryThread, "plots must be cleared from the main thread")
    val r = id.chunkRegion(config)
    for (cx <- r.minX to r.maxX) {
      for (cz <- r.minZ to r.maxZ) {
        bw.regenerateChunk(cx, cz)
      }
    }
  }

}

trait PlotHelper {
  
  def resolver: PlotWorldResolver
  def server: Server

  def isPlotWorld(w: World): Boolean = resolver(w).isDefined

  def isPlotWorld(loc: Location): Boolean = isPlotWorld(loc.getWorld)

  def inPlotWorld(p: Player)(pm: PlotWorld => Unit): Unit =
    resolver(p.getWorld).map(pm).getOrElse(p.sendMessage(err"You must be in a plot world to execute this command"))

  def withPlotStatus(p: Player, st: Status, err: Player => Unit)(f: (PlotWorld, Plot) => Unit): Unit =
    inPlotWorld(p) { w =>
      w.getPlot(w.getPlotId(p.getLocation)).fold(p.sendMessage(err"No plot found")) { plot =>
        if (plot.status(server, p).has(st)) f(w, plot)
        else err(p)
      }
    }

 def getPlot(loc: Location): Option[Plot] =
   resolver(loc.getWorld).flatMap(w => w.getPlot(w.getPlotId(loc)))

  private def failure(p: Player, s: String*): Result = { s.foreach(p sendMessage); Error }

  def has(st: Status, p: Player, loc: Location): Result = {
    resolver(loc.getWorld).fold[Result](False) { w =>
      if (!w.config.buildOnFloor && loc.getBlockY <= 0) failure(p, err"You cannot build on the floor level")
      else {
        if (p hasPermission "plot.admin") True
        else w.getPlot(w.getPlotId(loc))
            .fold(failure(p, err"You cannot build here")){ plot =>
          if (!plot.roadAccess && (w.config.isBorder(loc) || w.config.isPath(loc))) {
            failure(p, err"You cannot build here")
          } else {
            val pid = p.getUniqueId
            st match {
              case HelperOnline =>
                if (plot.isOwner(pid) || plot.isTrusted(pid)) True
                else if (plot.isHelper(pid)) {
                  if (server.isOnline(plot.owner)) True
                  else {
                    p.sendMessage(err"You cannot modify the plot while its owner is offline unless you are trusted")
                    Error
                  }
                } else {
                  False
                }
              case HelperOffline =>
                Result(plot.isOwner(pid) || plot.isHelper(pid) || plot.isTrusted(pid))
              case Owner =>
                Result(plot.isOwner(pid))
              case Trusted =>
                Result(plot.isOwner(pid) || plot.isTrusted(pid))
              case Banned =>
                Result(plot.isBanned(pid))
              case NotBanned =>
                Result(!plot.isBanned(pid))
              case Admin =>
                False
            }
          }}
      }
    }
  }

}

class PlotGenerator(c: PlotWorldConfig) extends ChunkGenerator {

  override def generateExtBlockSections(bw: World, random: Random, cx: Int, cz: Int, biomes: BiomeGrid): Array[Array[Short]] = {

    val result = new Array[Array[Short]](bw.getMaxHeight >> 4)

    for {
      x <- 0 until 16
      locX = (cx << 4) + x
      z <- 0 until 16
      locZ = (cz << 4) + z
    } yield setBlocks(x, z, locX, locZ)

    def setBlocks(x: Int, z: Int, locX: Int, locZ: Int): Unit = {
      Option(biomes).foreach(_.setBiome(x, z, c.style.biome))

      val path = c.isPath(locX, locZ)
      val border = c.isBorder(locX, locZ)

      if (path) {
        setBlock(result, x, c.pathHeight, z, c.style.path)
      } else if (border) {
        setBlock(result, x, c.borderHeight, z, c.style.border)
      } else {
        setBlock(result, x, c.topHeight, z, c.style.top)
      }

      setBlock(result, x, 0, z, c.style.bottom)

      for (y <- 1 until c.MaxY) {
        if (border && !path && y < c.borderHeight) {
          setBlock(result, x, y, z, c.style.wall)
        } else if (path || y < c.topHeight) {
          setBlock(result, x, y, z, c.style.filling)
        }
      }
    }

    result
  }

  def setBlock(result: Array[Array[Short]], x: Int, y: Int, z: Int, m: Material): Unit = {
    if (result(y >> 4) == null) result(y >> 4) = new Array[Short](4096)
    result(y >> 4)(((y & 0xF) << 8) | (z << 4) | x) = m.getId.toShort
  }

}