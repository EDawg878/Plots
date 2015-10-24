package com.edawg878.bukkit.listener

import java.nio.file.{Files, Path}
import java.util.UUID
import java.util.function.{Function, BiFunction}

import com.edawg878.bukkit.listener.VehicleTracker.UUIDBiMap
import com.edawg878.common.Server
import com.edawg878.common.Server._
import com.google.common.collect.HashBiMap
import org.bukkit.Material
import org.bukkit.entity.Entity
import org.bukkit.event.player.PlayerInteractEvent
import org.bukkit.event.{EventHandler, Listener}
import org.bukkit.event.vehicle.{VehicleDestroyEvent, VehicleCreateEvent}
import org.bukkit.inventory.ItemStack
import play.api.libs.json._
import com.edawg878.common.Color.Formatter
import org.bukkit.event.block.Action._
import scala.collection.JavaConversions._
import scala.concurrent.duration._

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
case class Vehicle(name: String,
                   fileName: String,
                   limit: Int,
                   entityTypes: Set[String],
                   types: Set[Material]) {

  def root(p: Plugin): Path = p.dataFolder.resolve("vehicles")

  def file(p: Plugin): Path = root(p).resolve(fileName)

  def matches(bv: org.bukkit.entity.Entity): Boolean = entityTypes.contains(bv.getType.getName)

  def matches(i: ItemStack): Boolean = types.contains(i.getType)

}

object VehicleTracker extends CustomReads {

  type UUIDBiMap = HashBiMap[UUID, Seq[UUID]]

  implicit val vehicleFormat = Json.format[Vehicle]

  def vehiclesFile(p: Plugin): Path = p.resolveFile("vehicles.json")

  def load(p: Plugin): Seq[VehicleTracker] = {
    val config = new Configuration[Seq[Vehicle]](p, vehiclesFile(p))
    val vehicles = config.parse
    vehicles.map { vehicle =>
      val file = vehicle.file(p)
      if (Files.notExists(file)) createEmptyJson(file)
      val uuids = new Configuration[UUIDBiMap](p, file)
      new VehicleTracker(uuids.parse, vehicle)
    }
  }

  def save(p: Plugin, t: VehicleTracker): Unit = {
    val json = Json.toJson[UUIDBiMap](t.map)
    val f = t.vehicle.file(p)
    val pretty = Json.prettyPrint(json)
    Files.write(f, pretty.getBytes)
  }

  def createEmptyJson(f: Path): Unit = {
    Files.createDirectories(f.getParent)
    Files.createFile(f)
    Files.write(f, "{}".getBytes)
  }

}

// BiMap[player_uuid, Seq[vehicle_uuid]]
case class VehicleTracker(map: UUIDBiMap, vehicle: Vehicle) extends CustomReads {

  def getVehicles(pid: UUID): Option[Seq[UUID]] = Option(map.get(pid))
  def getPlayer(vid: UUID): Option[UUID] = Option(map.inverse.get(vid))

  def add(pid: UUID, vid: UUID): Unit =
    map.compute(pid, new BiFunction[UUID, Seq[UUID], Seq[UUID]] {

      override def apply(k: UUID, v: Seq[UUID]): Seq[UUID] =
        if (v == null) Seq(vid) else v :+ vid

      override def andThen[V](after: Function[_ >: Seq[UUID], _ <: V]) = ???
    })

  def getCount(pid: UUID): Int = map.getOrDefault(pid, Seq()).length
  def canPlaceVehicle(pid: UUID): Boolean = getCount(pid) < vehicle.limit
  def remove(vid: UUID): Unit = map.inverse.remove(vid)

  // TODO: fix efficiency
  def containsVehicle(vid: UUID): Boolean = vehicles.contains(vid)
  def vehicles: java.util.Set[UUID] = map.inverse.keySet.flatten

  def players: java.util.Set[UUID] = map.keySet
  def save(p: Plugin): Unit = {
    val f = vehicle.file(p)
    val json = Json.toJson[UUIDBiMap](map)
    val pretty = Json.prettyPrint(json)
    Files.write(f, pretty.getBytes)
  }

}

class VehicleCleaner(server: Server, bukkitServer: org.bukkit.Server, trackers: Seq[VehicleTracker]) extends Schedulable {

  override val period = 60 seconds

  override def run(): Unit = {
    for {
      w <- bukkitServer.getWorlds
      e <- w.getEntities
      uuid = e.getUniqueId
      t <- trackers
      v = t.vehicle
      if v.matches(e)
      if !t.containsVehicle(uuid)
    } yield clean(t, v, e)
  }

  def clean(tracker: VehicleTracker, vehicle: Vehicle, entity: Entity): Unit = {
    val uuid = entity.getUniqueId
    for {
      pid <- tracker.getPlayer(uuid)
      p <- server.getPlayer(pid)
    } yield p.sendMessage(err"Your ${vehicle.name} has been either lost or destroyed, you may now place another")
    entity.remove()
  }

}

object VehicleListener {

  def load(server: Server, trackers: Seq[VehicleTracker]): VehicleListener = {
    val map = trackers.map(t => (t.vehicle, t)).toMap
    new VehicleListener(server, map, None)
  }

}

case class VehicleListener(server: Server, trackers: Map[Vehicle, VehicleTracker], var lastPlayer: Option[UUID]) extends Listener {

  def setPlayer(pid: UUID): Unit =
    lastPlayer = Some(pid)

  def findVehicle(bv: org.bukkit.entity.Vehicle): Option[Vehicle] =
    trackers.keys.find(_.matches(bv))

  def isVehicle(i: ItemStack): Boolean =
    trackers.keys.exists(_.types.contains(i.getType))

  @EventHandler
  def onVehicleCreate(ev: VehicleCreateEvent): Unit = {
    def checkVehiclePlace(p: Server.Player, t: VehicleTracker, v: Vehicle): Unit = {
      if (!t.canPlaceVehicle(p.id)) {
        ev.getVehicle.remove()
        p.sendMessage(err"Please remove your ${v.name} before placing another")
      } else {
        t.add(p.id, ev.getVehicle.getUniqueId)
      }
    }
    for {
      last <- lastPlayer
      player <- server.getPlayer(last)
      vehicle <- findVehicle(ev.getVehicle)
      tracker <- trackers.get(vehicle)
    } yield checkVehiclePlace(player, tracker, vehicle)
  }

  @EventHandler
  def onVehicleDestroy(ev: VehicleDestroyEvent): Unit =
    findVehicle(ev.getVehicle)
      .flatMap(trackers.get)
      .foreach(_.remove(ev.getVehicle.getUniqueId))

  @EventHandler
  def onPlayerInteract(ev: PlayerInteractEvent): Unit = {
    if (ev.getAction == RIGHT_CLICK_BLOCK) {
      Option(ev.getItem)
        .filter(isVehicle)
        .foreach(i => setPlayer(ev.getPlayer.getUniqueId))
    }
  }

}
