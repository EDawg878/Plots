package com.edawg878.bukkit

import java.nio.file.Path

import com.edawg878.common.Server._
import org.bukkit.configuration.file.{YamlConfiguration, FileConfiguration}
import scala.collection.JavaConverters._
import scala.util.Try

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
/*
object BukkitConfiguration {

  def load(configPath: Path): Configuration = new BukkitConfiguration(configPath, YamlConfiguration.loadConfiguration(configPath.toFile))

}

class BukkitConfigurationSection(parent: BukkitConfiguration, section: org.bukkit.configuration.ConfigurationSection)
  extends ConfigurationSection {

  override def getSection(path: String): Option[ConfigurationSection] =
    Option(section.getConfigurationSection(path)).map(new BukkitConfigurationSection(parent, _))

  override def getKeys: Iterable[String] = section.getKeys(false).asScala

  override def get(path: String): Option[YamlValue] = Option(section.get(path)).flatMap(Yaml.readObj)

  override def set(path: String, value: YamlValue): Unit = section.set(path, value)
}

class BukkitConfiguration(val configPath: Path, config: FileConfiguration)
  extends Configuration(configPath) {

  def reload: Configuration = BukkitConfiguration.load(configPath)

  override def save(): Unit = config.save(configPath.toFile)

  override def get(path: String): Option[YamlValue] = Option(config.get(path)).flatMap(Yaml.readObj)

  override def getSection(path: String): Option[ConfigurationSection] =
    Option(config.getConfigurationSection(path)).map(new BukkitConfigurationSection(this, _))

  override def getKeys: Iterable[String] = config.getKeys(false).asScala

  override def set(path: String, yaml: YamlValue): Unit = config.set(path, yaml.value)
}
*/