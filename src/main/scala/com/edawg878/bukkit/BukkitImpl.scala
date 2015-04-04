package com.edawg878.bukkit

import java.io.InputStream
import java.nio.file.Path
import java.util.UUID
import java.util.logging.Logger

import com.edawg878.common.Server
import com.edawg878.common.Server._
import org.bukkit.configuration.file._
import org.bukkit.entity.Player

import scala.collection.JavaConverters._

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object BukkitImpl {

  class BukkitConfigurationSection(section: org.bukkit.configuration.ConfigurationSection) extends ConfigurationSection {

    override def get(path: String): AnyRef = section.get(path)

    override def getSection(path: String): ConfigurationSection =
      new BukkitConfigurationSection(section.getConfigurationSection(path))

    override def set(path: String, value: AnyVal): Unit = section.set(path, value)

    override def get[T](path: String, default: T): Unit = section.get(path, default)

    override def getKeys: Iterable[String] = section.getKeys(false).asScala
  }

  private def load(path: Path): FileConfiguration =
    YamlConfiguration.loadConfiguration(path.toFile)

  class BukkitConfiguration(plugin: Plugin, name: String, var config: FileConfiguration) extends Configuration(plugin, name) {

    def this(plugin: Plugin, name: String) =
      this(plugin, name, load(plugin.resolveFile(name)))

    def reload: Configuration =
      new BukkitConfiguration(plugin, name, load(path))

    override def save(): Unit = config.save(path.toFile)

    override def get(path: String): AnyRef = config.get(path)

    override def getSection(path: String): ConfigurationSection =
      new BukkitConfigurationSection(config.getConfigurationSection(path))

    override def set(path: String, value: AnyVal): Unit = config.set(path, value)

    override def get[T](path: String, default: T): Unit = config.get(path, default)

    override def getKeys: Iterable[String] = config.getKeys(false).asScala

  }

  implicit class BukkitPlugin(val plugin: org.bukkit.plugin.Plugin) extends Plugin {

    override def getDataFolder: Path = plugin.getDataFolder.toPath

    override def getLogger: Logger = plugin.getLogger

    override def getResource(name: String): InputStream = plugin.getResource(name)
  }

  implicit class BukkitConsole(val console: org.bukkit.command.CommandSender) extends Console {

    override def sendMessage(message: String): Unit = console.sendMessage(message)

    override def hasPermission(permission: String): Boolean = console.hasPermission(permission)

  }

  implicit class BukkitPlayer(val player: Player) extends Server.Player {

    override def getName: String = player.getName

    override def getUniqueId: UUID = player.getUniqueId

    override def sendMessage(message: String): Unit = player.sendMessage(message)

    override def hasPermission(permission: String): Boolean = player.hasPermission(permission)

    override def getDisplayName: String = player.getDisplayName

  }

}
