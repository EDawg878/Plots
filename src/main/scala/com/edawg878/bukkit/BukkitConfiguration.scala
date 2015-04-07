package com.edawg878.bukkit

import com.edawg878.common.Server.{Configuration, Plugin, ConfigurationSection}
import org.bukkit.configuration.file.{YamlConfiguration, FileConfiguration}
import scala.collection.JavaConverters._

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
class BukkitConfigurationSection(section: org.bukkit.configuration.ConfigurationSection)
  extends ConfigurationSection {

  override def get(path: String): AnyRef = section.get(path)

  override def getSection(path: String): ConfigurationSection =
    new BukkitConfigurationSection(section.getConfigurationSection(path))

  override def set(path: String, value: AnyVal): Unit = section.set(path, value)

  override def get[T](path: String, default: T): Unit = section.get(path, default)

  override def getKeys: Iterable[String] = section.getKeys(false).asScala
}

class BukkitConfiguration(plugin: Plugin, name: String, var config: FileConfiguration)
  extends Configuration(plugin, name) {

  def this(plugin: Plugin, name: String) = this(plugin, name, YamlConfiguration.loadConfiguration(plugin.resolveFile(name).toFile))

  def reload: Configuration = new BukkitConfiguration(plugin, name, YamlConfiguration.loadConfiguration(path.toFile))

  override def save(): Unit = config.save(path.toFile)

  override def get(path: String): AnyRef = config.get(path)

  override def getSection(path: String): ConfigurationSection =
    new BukkitConfigurationSection(config.getConfigurationSection(path))

  override def set(path: String, value: AnyVal): Unit = config.set(path, value)

  override def get[T](path: String, default: T): Unit = config.get(path, default)

  override def getKeys: Iterable[String] = config.getKeys(false).asScala

}
