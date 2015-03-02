package com.edawg878.bungee

import java.io.InputStream
import java.nio.file.Path
import java.util.UUID
import java.util.logging.Logger

import scala.collection.JavaConverters._

import com.edawg878.common._
import net.md_5.bungee.api.chat.TextComponent
import net.md_5.bungee.api.connection.ProxiedPlayer
import net.md_5.bungee.api.CommandSender
import net.md_5.bungee.config.{YamlConfiguration, ConfigurationProvider}

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
class BungeeImpl {

  val ConfigProvider = ConfigurationProvider.getProvider(classOf[YamlConfiguration])

  private def load(path: Path): net.md_5.bungee.config.Configuration =
    ConfigProvider.load(path.toFile)

  class BungeeConfiguration(plugin: Plugin, name: String, config: net.md_5.bungee.config.Configuration) extends Configuration(plugin, name) {

    def this(plugin: Plugin, name: String) =
      this(plugin, name, load(plugin.resolveFile(name)))

    override def reload: Configuration =
      new BungeeConfiguration(plugin, name, load(path))

    override def save: Unit = ConfigProvider.save(config, path.toFile)

    override def get(path: String): AnyRef = config.get(path)

    override def getSection(path: String): Configuration =
      new BungeeConfiguration(plugin, name)

    override def set(path: String, value: AnyVal): Unit = config.set(path, value)

    override def get[T](path: String, default: T): Unit = config.get(path, default)

    override def getKeys: Iterable[String] = config.getKeys.asScala

  }

  implicit class BungeePlugin(val plugin: net.md_5.bungee.api.plugin.Plugin) extends Plugin {

    override def getDataFolder: Path = plugin.getDataFolder.toPath

    override def getLogger: Logger = plugin.getLogger

    override def getResource(name: String): InputStream = plugin.getResourceAsStream(name)

  }

  implicit class BungeeConsole(val console: CommandSender) extends Console {

    override def sendMessage(message: String): Unit = console.sendMessage(message)

    override def hasPermission(permission: String): Boolean = console.hasPermission(permission)

  }

  implicit class BungeeUser(val player: ProxiedPlayer) extends User {

    override def getName: String = player.getName

    override def getUniqueId: UUID = player.getUniqueId

    override def sendMessage(message: String): Unit = player.sendMessage(TextComponent.fromLegacyText(message): _*)

    override def hasPermission(permission: String): Boolean = player.hasPermission(permission)

    override def getDisplayName: String = player.getDisplayName

  }

}
