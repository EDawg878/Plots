package com.edawg878.bukkit.listener

import java.nio.file.{Files, Path}
import java.util.UUID

import com.edawg878.bukkit.listener.VehicleTracker.UUIDBiMap
import com.edawg878.common.Server
import com.edawg878.common.Server.Server
import com.edawg878.common.Server.{CustomReads, Server, Configuration, Plugin}
import com.google.common.collect.HashBiMap
import org.bukkit.{Bukkit, Material}
import org.bukkit.entity.{EntityType, Player}
import org.bukkit.event.player.PlayerInteractEvent
import org.bukkit.event.{EventHandler, Listener}
import org.bukkit.event.vehicle.{VehicleDestroyEvent, VehicleCreateEvent}
import org.bukkit.inventory.ItemStack
import play.api.libs.json._
import scala.collection.JavaConverters._
import com.edawg878.common.Color.Formatter
import org.bukkit.event.block.Action._
import org.bukkit.Material._

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
case class Vehicle(name: String,
                   fileName: String,
                   entityTypes: Set[String],
                   types: Set[Material]) {

  def root(p: Plugin): Path = p.dataFolder.resolve("vehicles")

  def file(p: Plugin): Path = root(p).resolve(fileName)

  def matches(bv: org.bukkit.entity.Vehicle): Boolean = entityTypes.contains(bv.getType.getName)

  def matches(i: ItemStack): Boolean = types.contains(i.getType)

}

object VehicleTracker extends CustomReads {

  type UUIDBiMap = HashBiMap[UUID, UUID]

  implicit val vehicleFormat = Json.format[Vehicle]

  def vehiclesFile(p: Plugin): Path = p.resolveFile("vehicles.json")

  def config(p: Plugin) = new Configuration[Seq[Vehicle]](p, vehiclesFile(p))

  def load(p: Plugin): Seq[VehicleTracker] = {
    val vehicles = config(p)
    vehicles.saveDefault()
    vehicles.parse.map { vehicle =>
      val file = vehicle.file(p)

      if (Files.notExists(file))
        createEmptyJson(file)

      val uuids = new Configuration[UUIDBiMap](p, file)
      new VehicleTracker(uuids.parse, vehicle)
    }
  }

  def save(p: Plugin, t: VehicleTracker): Unit = {
    val json = Json.toJson[UUIDBiMap](t.m)
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

case class VehicleTracker(m: UUIDBiMap, vehicle: Vehicle) extends CustomReads {

  def add(pid: UUID, vid: UUID): Unit = m.put(pid, vid)
  def remove(vid: UUID): Unit = m.inverse.remove(vid)
  def containsVehicle(vid: UUID): Boolean = m.inverse.containsKey(vid)
  def containsOwner(vid: UUID): Boolean = m.containsKey(vid)
  def vehicles: Iterable[UUID] = m.inverse.keySet.asScala
  def players: Iterable[UUID] = m.keySet.asScala
  def save(p: Plugin): Unit = {
    val f = vehicle.file(p)
    val json = Json.toJson[UUIDBiMap](m)
    val pretty = Json.prettyPrint(json)
    Files.write(f, pretty.getBytes)
  }

}

object VehicleListener {

  def load(server: Server, vt: Seq[VehicleTracker]): VehicleListener = {
    val map = vt.map{ tracker => (tracker.vehicle, tracker) }.toMap
    new VehicleListener(server, map, None)
  }

}

case class VehicleListener(server: Server, vMap: Map[Vehicle, VehicleTracker], var lastPlayer: Option[UUID]) extends Listener {

  def setLastPlayer(pid: UUID): Unit =
    lastPlayer = Some(pid)

  def find(bv: org.bukkit.entity.Vehicle): Option[Vehicle] =
    vMap.keys.find(_.matches(bv))

  def isVehicle(i: ItemStack): Boolean =
    vMap.keys.exists(_.types.contains(i.getType))

  @EventHandler
  def onVehicleCreate(ev: VehicleCreateEvent): Unit = {

    def checkVehiclePlace(p: Server.Player, t: VehicleTracker, v: Vehicle): Unit = {
      if (t.containsOwner(p.id)) {
        ev.getVehicle.remove()
        p.sendMessage(err"Please remove your ${v.name} before placing another")
      } else {
        t.add(p.id, ev.getVehicle.getUniqueId)
      }
    }

    for {
      last <- lastPlayer
      player <- server.getPlayer(last)
      vehicle <- find(ev.getVehicle)
      tracker <- vMap.get(vehicle)
    } yield checkVehiclePlace(player, tracker, vehicle)
  }


  @EventHandler
  def onVehicleDestroy(ev: VehicleDestroyEvent): Unit =
    find(ev.getVehicle).flatMap(vMap.get)
      .foreach(_.remove(ev.getVehicle.getUniqueId))

  @EventHandler
  def onPlayerInteract(ev: PlayerInteractEvent): Unit = {
    if (ev.getAction == RIGHT_CLICK_BLOCK) {
      Option(ev.getItem)
        .filter(isVehicle)
        .foreach(i => setLastPlayer(ev.getPlayer.getUniqueId))
    }
  }

}
