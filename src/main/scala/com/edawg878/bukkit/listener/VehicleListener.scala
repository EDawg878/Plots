package com.edawg878.bukkit.listener

import java.nio.file.{Files, Path}
import java.util.UUID

import com.edawg878.common.Server.{Server, Configuration, Plugin}
import com.google.common.collect.HashBiMap
import org.bukkit.Material
import org.bukkit.entity.Player
import org.bukkit.event.player.PlayerInteractEvent
import org.bukkit.event.{EventHandler, Listener}
import org.bukkit.event.vehicle.{VehicleDestroyEvent, VehicleCreateEvent}
import org.bukkit.inventory.ItemStack
import scala.collection.JavaConverters._
import com.edawg878.common.Color.Formatter
import org.bukkit.event.block.Action._
import org.bukkit.Material._

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object VehicleListener {

  type UUIDBiMap = HashBiMap[UUID, UUID]

  case class Vehicle(name: String,
                     fileName: String,
                     entityClass: Class,
                     types: Set[Material]) {

    def file(p: Plugin): Path = p.dataFolder.resolve(fileName)

    def matches(s: String): Boolean = name.toLowerCase == s.toLowerCase

    def matches(bv: org.bukkit.entity.Vehicle): Boolean = bv.getClass.isAssignableFrom(entityClass)

  }

  /*
  case object Boat extends Vehicle {
    val types = Set(BOAT)
    val vehicle = org.bukkit.entity.Boat
    val fileName = "boats.json"
    val name = "boat"
  }

  case object Cart extends Vehicle {
    val types = Set(MINECART, POWERED_MINECART, EXPLOSIVE_MINECART, COMMAND_MINECART, HOPPER_MINECART, STORAGE_MINECART)
    val vehicle = org.bukkit.entity.Minecart
    val fileName = "carts.json"
    val name = "cart"
  }*/

  object VehicleTracker {

    def load(p: Plugin): Seq[VehicleTracker] = {
      val f = p.resolveFile("vehicles.json")
      val c = new Configuration[Seq[Vehicle]](p, f)
      c.saveDefault()
      c.parse.map { v =>
        val input = v.file(p)
        val uuidConfig = new Configuration[UUIDBiMap](p, input)
        if (Files.notExists(uuidConfig.path)) createFile(p, v)
        new VehicleTracker(uuidConfig.parse, v)
      }
    }

    def createFile(p: Plugin, v: Vehicle): Unit = {
      val emptyJsonFile = p.resolveFile("empty.json")
      p.saveRootResource(emptyJsonFile)
      Files.move(emptyJsonFile, v.file(p))
    }

  }

  case class VehicleTracker(m: UUIDBiMap, vehicle: Vehicle) {

    //val vehicles: Seq[Vehicle] = Seq(Cart, Boat)
    //val types: Set[Material] = vehicles.reduce{ case (a, b) => a.types ++ b.types }

    def matches(s: String): Boolean =
      vehicle.name.toLowerCase == s.toLowerCase

    def matches(v: org.bukkit.entity.Vehicle): Boolean =
      v.getClass.isAssignableFrom(vehicle.entityClass)

    def matches(i: ItemStack): Boolean = vehicle.types.contains(i.getType)

    def add(pid: UUID, vid: UUID): Unit = m.put(pid, vid)
    def remove(vid: UUID): Unit = m.inverse.remove(vid)
    def containsVehicle(vid: UUID): Boolean = m.inverse.containsKey(vid)
    def containsOwner(vid: UUID): Boolean = m.containsKey(vid)
    def vehicles: Iterable[UUID] = m.inverse.keySet.asScala
    def players: Iterable[UUID] = m.keySet.asScala

  }

  object VehicleListener {

    def load(server: Server, vt: Seq[VehicleTracker]): VehicleListener = {
      val map = vt.map{ tracker => (tracker.vehicle, tracker) }.toMap
      new VehicleListener(server, map, None)
    }

  }

  case class VehicleListener(server: Server, vMap: Map[Vehicle, VehicleTracker], var lastPlayer: Option[UUID]) extends Listener {

    def setLastPlayer(p: Player): Unit = lastPlayer = Some(p.getUniqueId)

    def find(bv: org.bukkit.entity.Vehicle): Option[Vehicle] =
      vMap.keys.find(v => v.matches(bv))

    def isVehicle(i: ItemStack): Boolean =
      vMap.keys.exists(_.types.contains(i.getType))

    @EventHandler
    def onVehicleCreate(ev: VehicleCreateEvent): Unit =
      lastPlayer.flatMap(server.getPlayer).foreach(p =>
        find(ev.getVehicle).foreach(vehicle =>
            vMap.get(vehicle).foreach { tracker =>
              val vid = ev.getVehicle.getUniqueId
              if (tracker.containsVehicle(vid)) {
                tracker.add(p.id, vid)
              } else {
                ev.getVehicle.remove()
                p.sendMessage(err"Please remove your current ${vehicle.name} before placing another")
              }
            }
        ))

    @EventHandler
    def onVehicleDestroy(ev: VehicleDestroyEvent): Unit =
      find(ev.getVehicle).flatMap(vMap.get)
        .foreach(_.remove(ev.getVehicle.getUniqueId))

    @EventHandler
    def onPlayerInteract(ev: PlayerInteractEvent): Unit = {
      if (ev.getAction == RIGHT_CLICK_BLOCK) {
       Option(ev.getItem)
         .filter(isVehicle)
         .foreach(i => setLastPlayer(ev.getPlayer))
      }
    }

  }


}
