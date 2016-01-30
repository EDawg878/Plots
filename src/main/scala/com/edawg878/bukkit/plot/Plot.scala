package com.edawg878.bukkit.plot

import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalAmount
import java.time.{Instant, LocalDate}
import java.util.{Random, UUID}

import com.edawg878.bukkit.BukkitConversions._
import com.edawg878.bukkit.plot.Plot._
import com.edawg878.common.Color.Formatter
import com.edawg878.common.PlotRepository
import com.edawg878.common.Server.Server
import org.bukkit.Material._
import org.bukkit._
import org.bukkit.block.Biome
import org.bukkit.block.Biome._
import org.bukkit.entity.Player
import org.bukkit.generator.ChunkGenerator
import org.bukkit.generator.ChunkGenerator.BiomeGrid

import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

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

  val Center = Position(0, 0, 0)
  val ChunkSize = 16
  val ChunkExp = Math.getExponent(ChunkSize)
  val WorldExp = Math.getExponent(plotSize)
  val MaxY = math.max(pathHeight, math.max(borderHeight, topHeight))
  val MinY = if (buildOnFloor) 1 else 2

  def outer(id: PlotId): BlockRegion = {
    val minX = id.x << WorldExp
    val minZ = id.z << WorldExp
    val maxX = minX + plotSize - 1
    val maxZ = minZ + plotSize - 1
    new BlockRegion(minX, maxX, minZ, maxZ)
  }

  def inner(id: PlotId): BlockRegion = {
    val r = outer(id)
    val len = pathWidth
    val minX = r.minX + len
    val minZ = r.minZ + len
    val maxX = r.maxX - len
    val maxZ = r.maxZ - len
    new BlockRegion(minX, maxX, minZ, maxZ)
  }

  def border(id: PlotId): BlockRegion = {
    val r = outer(id)
    val len = pathWidth - 1
    val minX = r.minX + len
    val minZ = r.minZ + len
    val maxX = r.maxX - len
    val maxZ = r.maxZ - len
    new BlockRegion(minX, maxX, minZ, maxZ)
  }

  def isBorder(x: Int, z: Int): Boolean = {
    val id = PlotId.fromBlockLocation(this, x, z)
    val r = border(id)
    x == r.minX ||
      x == r.maxX ||
      z == r.minZ ||
      z == r.maxZ
  }

  def isPath(x: Int, z: Int): Boolean = {
    val id = PlotId.fromBlockLocation(this, x, z)
    val o = outer(id)
    val b = border(id)
    x > b.maxX && x <= o.maxX ||
      z > b.maxZ && z <= o.maxZ ||
      x < b.minX && x >= o.minX ||
      z < b.minZ && z >= o.minZ
  }

  def isBorder(loc: Location): Boolean = isBorder(loc.getBlockX, loc.getBlockZ)

  def isPath(loc: Location): Boolean = isPath(loc.getBlockX, loc.getBlockZ)

  def isRoad(loc: Location): Boolean = isPath(loc) || isBorder(loc)

}

case class Position(x: Int, y: Int, z: Int) {

  def add(x: Int, y: Int, z: Int): Position =
    copy(x = this.x + x, y = this.y + y, z = this.z + z)

}

case class Corners(bottom: Position, top: Position)

object BlockRegion {

  def create(x1: Int, x2: Int, z1: Int, z2: Int) =
    new BlockRegion(math.min(x1, x2), math.max(x1, x2), math.min(z1, z2), math.max(z1, z2))

  def between(p: Position, a: BlockRegion, b: BlockRegion): Boolean = {
    a.isInside(p.x, p.z) && b.isInside(p.x, p.z)
  }

}

trait Region {

  require(minX < maxX, "minX must be less than maxX")
  require(minZ < maxZ, "minZ must be less than maxZ")
  require(maxX > minX, "maxX must be greater than minX")
  require(maxZ > minZ, "maxZ must be greater than minZ")
  
  def minX: Int
  def maxX: Int
  def minZ: Int
  def maxZ: Int

  def isInside(x: Int, z: Int): Boolean = {
    x >= minX &&
      x <= maxX &&
      z >= minZ &&
      z <= maxZ
  }
  
  def isPast(x: Int, z: Int): Boolean = {
    x < minX ||
      x > maxX ||
      z < minZ ||
      z > maxZ
  }

}

case class BlockRegion(minX: Int, maxX: Int, minZ: Int, maxZ: Int) extends Region {
  
  def isInside(p: Position): Boolean = isInside(p.x, p.z)
  def isInside(loc: Location): Boolean = isInside(loc.getBlockX, loc.getBlockZ)
  
  def grow(that: BlockRegion): BlockRegion = {
    val minX = math.min(this.minX, that.minX)
    val maxX = math.max(this.maxX, that.maxX)
    val minZ = math.min(this.minZ, that.minZ)
    val maxZ = math.max(this.maxZ, that.maxZ)
    BlockRegion(minX, maxX, minZ, maxZ)
  }
  
}

object Border {

  def find(c: PlotWorldConfig, ids: Iterable[PlotId]): Border = {
    val regions = ids.map(_.region(c))
    val first = initial(c).copy(knockback = c.pathWidth - 1)
    if (regions.isEmpty) first
    else first.copy(region = regions.reduce((a, b) => a.grow(b)))
  }
  
  def create(c: PlotWorldConfig, id: PlotId): Border = Border(region = c.outer(id))
  def initial(c: PlotWorldConfig): Border = Border(region = c.outer(PlotId.fromPosition(c, c.Center)))
  
}

case class Border(knockback: Int = 0, region: BlockRegion) {
  
  def isPast(loc: Location): Boolean =
    region.isPast(loc.getBlockX, loc.getBlockZ)

  def knockback(loc: Location): Location = {
    val locX = loc.getX
    val locZ = loc.getZ
    val x =
      if (locX <= region.minX) region.minX + knockback
      else if (locX >= region.maxX) region.maxX - knockback
      else locX
    val z =
      if (locZ <= region.minZ) region.minZ + knockback
      else if (locZ >= region.maxZ) region.maxZ - knockback
      else locZ
    val adjusted = loc.clone
    adjusted.setX(x)
    adjusted.setZ(z)
    adjusted
  }

}

object PlotId {

  def parse(c: PlotWorldConfig, p: String): Option[PlotId] = {
    val s = p.split(";")
    Try(PlotId(s(0).toInt, s(1).toInt, c.name)).toOption
  }

  def fromPosition(c: PlotWorldConfig, p: Position) = this(p.x >> c.WorldExp, p.z >> c.WorldExp, c.name)

  def fromBlockLocation(c: PlotWorldConfig, locX: Int, locZ: Int) = this(locX >> c.WorldExp, locZ >> c.WorldExp, c.name)
  
  

}

case class PlotId(x: Int, z: Int, world: String) {

  def region(c: PlotWorldConfig): BlockRegion = {
    val blockX = x << c.WorldExp
    val blockZ = z << c.WorldExp
    val minX = blockX
    val maxX = blockX + c.plotSize
    val minZ = blockZ
    val maxZ = blockZ + c.plotSize
    BlockRegion(minX, maxX, minZ, maxZ)
  }

  def chunkRegion(c: PlotWorldConfig): BlockRegion = {
    val exp = c.WorldExp - c.ChunkExp
    val chunks = (c.plotSize >> 4) - 1
    val minX = x << exp
    val minZ = z << exp
    val maxX = minX + chunks
    val maxZ = minZ + chunks
    BlockRegion(minX, maxX, minZ, maxZ)
  }

  def distanceSq(id: PlotId): Double = math.pow(id.x - x, 2) + math.pow(id.z - z, 2)

  def distance(id: PlotId): Double = math.sqrt(distanceSq(id))

  override def toString: String = Seq(x, z).mkString(";")

}

object Plot {

  sealed trait Status {
    def rank: Int
    def has(that: Status): Boolean = this.rank >= that.rank
    def >(that: Status): Boolean = this.rank > that.rank
    def >=(that: Status): Boolean = this.rank >= that.rank
    def <(that: Status): Boolean = this.rank < that.rank
    def <=(that: Status): Boolean = this.rank <= that.rank
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

  /*
  object Result {
    def apply(b: Boolean) = if (b) True else False
  }

  sealed trait Result
  case object Error extends Result
  case object True extends Result
  case object False extends Result
  */

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

trait PlotWorldResolver {

  def apply(s: String): Option[PlotWorld]

  def apply(bw: World): Option[PlotWorld] = apply(bw.getName)

}

object PlotWorld {

  def load(c: PlotWorldConfig, db: PlotRepository): Future[PlotWorld] =
    db.findAll(c.name)
        .map(_.map(p => p.id -> p))
        .map(TrieMap.apply)
        .map(plots => PlotWorld(c, plots, Border.find(c, plots.keys)))

}

case class PlotWorld(config: PlotWorldConfig, plots: TrieMap[PlotId, Plot], var border: Border) {

  def update(plot: Plot): Unit = plots.put(plot.id, plot)

  def getPlotId(loc: Location): PlotId = PlotId.fromBlockLocation(config, loc.getBlockX, loc.getBlockZ)

  def getPlot(id: PlotId): Option[Plot] = plots.get(id)

  def claim(p: Player, id: PlotId): Plot = {
    val plot = Plot(p.getUniqueId, id)
    plots.put(id, plot)
    val region = plot.id.region(config)
    border = border.copy(region = border.region.grow(region))
    plot
  }

  def unclaim(id: PlotId): Option[Plot] = plots.remove(id)

  def getHomeLocation(bw: World, id: PlotId): Location = {
    val region = config.outer(id)
    val x = region.minX + config.pathWidth
    val z = region.minZ + config.pathWidth
    val y = bw.getHighestBlockYAt(x, z)
    val yaw = -45f
    val pitch = 0f
    new Location(bw, x, y, z, yaw, pitch)
  }
  
  def getHomes(pid: UUID): Seq[Plot] = plots.values.filter(_.isOwner(pid)).toSeq.sorted

  def getHome(pid: UUID, n: Int): Option[Plot] = Try(getHomes(pid)(n - 1)).toOption
  
  def getHome(pid: UUID, s: String): Option[Plot] = getHomes(pid).find(_.alias.contains(s))
  
  def isInside(l: Location, id: PlotId): Boolean =
    l.getWorld.getName == config.name && config.outer(id).isInside(l)

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

  def inPlotWorld(p: Player)(f: PlotWorld => Unit): Unit =
    resolver(p.getWorld).fold(p.sendMessage(err"You must be in a plot world to execute this command"))(f)

  def inPlot(p: Player, loc: Location)(f: (PlotWorld, Plot) => Unit): Unit =
    inPlotWorld(p)(w => w.getPlot(w.getPlotId(loc)).fold(p.sendMessage(err"No plot found"))(plot => (w, plot)))

  def inPlot(p: Player)(f: (PlotWorld, Plot) => Unit): Unit = inPlot(p, p.getLocation)(f)

  def withPlotStatus(p: Player, st: Status, err: Player => Unit)(f: (PlotWorld, Plot) => Unit): Unit =
    inPlotWorld(p) { w =>
      w.getPlot(w.getPlotId(p.getLocation)).fold(p.sendMessage(err"No plot found")) { plot =>
        if (plot.status(server, p).has(st)) f(w, plot)
        else err(p)
      }
    }

 def getPlot(loc: Location): Option[Plot] =
   resolver(loc.getWorld).flatMap(w => w.getPlot(w.getPlotId(loc)))

  def has(st: Status, p: Player, loc: Location, s: String): Either[String, Unit] = {
    def eval(cond: Boolean) = if (cond) Right() else Left(s)
    resolver(loc.getWorld) map { w =>
      if (!w.config.buildOnFloor && loc.getBlockY <= 0) Left(err"You cannot build on the floor level")
      else if (p.hasPermission("plot.admin")) Right()
      else {
        w.getPlot(w.getPlotId(loc)) map { plot =>
          if (!plot.roadAccess && w.config.isRoad(loc)) Left(err"You cannot build here")
          else {
            val pid = p.getUniqueId
            st match {
              case HelperOnline =>
                if (plot.isOwner(pid) || plot.isTrusted(pid)) Right()
                else if (plot.isHelper(pid))
                  if (server.isOnline(plot.owner)) Right()
                  else Left(err"You cannot modify the plot while its owner is offline unless you are trusted")
                else Left(s)
              case HelperOffline =>
                eval(plot.isOwner(pid) || plot.isHelper(pid) || plot.isTrusted(pid))
              case Owner =>
                eval(plot.isOwner(pid))
              case Trusted =>
                eval(plot.isOwner(pid) || plot.isTrusted(pid))
              case Banned =>
                eval(plot.isBanned(pid))
              case NotBanned =>
                eval(!plot.isBanned(pid))
              case Admin =>
                Left(s)
            }
          }
        } getOrElse(Left(err"You cannot build here"))
      }
    } getOrElse(Left(s))
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