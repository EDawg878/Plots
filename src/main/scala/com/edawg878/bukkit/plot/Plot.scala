package com.edawg878.bukkit.plot

import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalAmount
import java.time.{Duration, LocalDate, Instant}
import java.util.UUID

import com.edawg878.common.Server.Configuration
import org.bukkit.{Bukkit, World, Location, Material}
import org.bukkit.Material._
import org.bukkit.block.Biome
import org.bukkit.block.Biome._

import scala.util.Try

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */


case class PlotStyle(border: Material = STEP,
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
                      pathHeight: Int = 64,
                      pathWidth: Int = 3,
                      buildOnFloor: Boolean = false,
                      border: Boolean = true) {

    val ChunkSize: Int = 16
    val ChunkExp: Int = Math.getExponent(ChunkSize)
    val WorldExp: Int = Math.getExponent(plotSize)

}

case class Corners(bottom: Location, top: Location)

case class Region(minX: Int, maxX: Int, minZ: Int, maxZ: Int)

object ChunkRegion {

  def apply(id: PlotId): ChunkRegion = {
    val w = id.world
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

  def parse(world: PlotWorld, input: String): Option[PlotId] = {
    val s = input.split(";")
    Try(PlotId(s(0).toInt, s(1).toInt, world)).toOption
  }
  
}

case class PlotId(x: Int, z: Int, world: PlotWorld) {
  
  lazy val blockX: Int = x << world.WorldExp
  lazy val blockZ: Int = z << world.WorldExp
  
  def bukkitWorld: World = Option(Bukkit.getWorld(world.name))
    .getOrElse(throw new RuntimeException(s"Could not find Bukkit world ${world.name}"))
  
  def corners: Corners = {
    val w = bukkitWorld
    val bottom = new Location(w, blockX, 0, blockZ)
    val top =  new Location(w, blockX + world.plotSize - 1, w.getMaxHeight, blockZ + world.plotSize - 1)
    Corners(bottom, top)
  }
  
  def region: Region = {
    val size = world.plotSize - 1
    Region(minX =  blockX, maxX = blockX + size, minZ = blockZ, maxZ = blockZ + size)
  }

  def isInside(loc: Location): Boolean = isInside(loc.toVector)

  def isInside(loc: org.bukkit.util.Vector): Boolean = {
    val c = corners
    loc.getBlockX >= c.bottom.getBlockX &&
      loc.getBlockX <= c.top.getBlockX &&
      loc.getBlockY >= c.bottom.getBlockY &&
      loc.getBlockY <= c.top.getBlockY &&
      loc.getBlockZ >= c.bottom.getBlockZ &&
      loc.getBlockZ <= c.top.getBlockZ
  }

  def distanceSq(id: PlotId): Double = math.pow(id.x - x, 2) + math.pow(id.z - z, 2)

  def distance(id: PlotId): Double = math.sqrt(distance(id))

  override def toString: String = x + ";" + z

}

object Plot {

  val ExpirationTime: TemporalAmount = Duration.ofDays(30)

  private def currentExpiration: LocalDate = LocalDate.now.plus(ExpirationTime)

  def apply(id: PlotId, owner: UUID): Plot = new Plot(
    id = id,
    owner = owner,
    timeClaimed = Instant.now(),
    expirationDate = currentExpiration
  )

}

case class Plot(id: PlotId,
                owner: UUID,
                timeClaimed: Instant,
                expirationDate: LocalDate,
                protect: Boolean = false,
                closed: Boolean = false,
                roadAccess: Boolean = false,
                helpers: Set[UUID] = Set.empty,
                trusted: Set[UUID] = Set.empty,
                banned: Set[UUID] = Set.empty)
  extends Ordered[Plot] {

  val ExpirationFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("MM/dd/yy")

  def resetExpire: Plot = copy(expirationDate = Plot.currentExpiration)

  def formattedExpirationDate = ExpirationFormatter.format(expirationDate)

  def isExpired: Boolean = !protect && LocalDate.now.isAfter(expirationDate)

  def removeAdded(id: UUID): Plot = copy(helpers = helpers - id, trusted = trusted - id)

  def removeBanned(id: UUID): Plot = copy(banned = banned - id)

  def clearAdded: Plot = copy(helpers = Set.empty, trusted = Set.empty)

  def clearBanned: Plot = copy(banned = Set.empty)

  def toggleRoadAccess: Plot = copy(roadAccess = !roadAccess)

  def ids: Set[UUID] = helpers ++ trusted ++ banned + owner

  override def compare(that: Plot): Int = timeClaimed compareTo that.timeClaimed
}

case class Border(dist: Int,
                  maxDistX: Int = 0,
                  minDistX: Int = 0,
                  maxDistZ: Int = 0,
                  minDistZ: Int = 0) {

  def this(w: PlotWorld, threshold: Int) = this(dist = w.plotSize * threshold)
  
  def recalculate(id: PlotId): Border = {
    val r = id.region
    copy(minDistX = minDistX.min(r.minX),
      maxDistX = maxDistX.max(r.maxX),
      minDistZ = minDistZ.min(r.minZ),
      maxDistZ = maxDistZ.max(r.maxZ)
    )
  }
  
  def isPast(loc: Location): Boolean =
    loc.getBlockX - dist > maxDistX ||
      loc.getBlockX + dist < minDistX ||
      loc.getBlockZ - dist > maxDistZ ||
      loc.getBlockZ + dist < minDistZ

}

case class PlotWorldConfig(config: Configuration, worlds: Map[String, PlotWorld]) {

  def reload: PlotWorldConfig = {
    val section = config.getSection("worlds")
    ???
  }


}