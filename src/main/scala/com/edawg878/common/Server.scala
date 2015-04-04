package com.edawg878.common

import java.io.{InputStream, IOException}
import java.nio.file.{Files, Path}
import java.util.UUID
import java.util.logging.Logger

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Server {

  trait Plugin {

    def getDataFolder: Path

    def resolveFile(name: String) = getDataFolder.resolve(name)

    @throws(classOf[IOException])
    def saveResource(in: InputStream, out: String): Unit = Files.copy(in, resolveFile(out))

    def getResource(name: String): InputStream

    def getLogger: Logger

  }

  trait ConfigurationSection {

    def get(path: String): AnyRef

    def get[T](path: String, default: T)

    def set(path: String, value: AnyVal)

    def getSection(path: String): ConfigurationSection

    def getKeys: Iterable[String]

  }

  abstract class Configuration(plugin: Plugin, name: String) extends ConfigurationSection {

    lazy val path = plugin.resolveFile(name)

    def reload: Configuration

    def save(): Unit

    def saveDefault(): Unit = {
      if (Files.notExists(path)) {
        Files.createDirectories(path.getParent)
        plugin.saveResource(plugin.getResource(name), name)
      }
    }

    def getSection(path: String): ConfigurationSection

  }

  trait Console {

    def sendMessage(message: String)

    def sendMessage(messages: String*): Unit = messages.foreach(sendMessage)

    def hasPermission(permission: String): Boolean

  }

  trait Player {

    def sendMessage(message: String)

    def sendMessage(messages: String*): Unit = messages.foreach(sendMessage)

    def hasPermission(permission: String): Boolean

    def getDisplayName: String

    def getName: String

    def getUniqueId: UUID

  }

}
