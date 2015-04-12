package com.edawg878.bungee

import java.nio.file.Path

import com.edawg878.common.Server.{Yaml, YamlValue, ConfigurationSection, Configuration}
import net.md_5.bungee.config.{YamlConfiguration, ConfigurationProvider}
import scala.collection.JavaConverters._

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
/*
object BungeeConfiguration {

  val provider = ConfigurationProvider.getProvider(classOf[YamlConfiguration])

  def load(configPath: Path): Configuration = new BungeeConfiguration(configPath, provider.load(configPath.toFile))

}

class BungeeConfiguration private(configPath: Path, config: net.md_5.bungee.config.Configuration)
  extends Configuration(configPath) {

  override def reload: Configuration = BungeeConfiguration.load(configPath)

  override def save(): Unit = BungeeConfiguration.provider.save(config, configPath.toFile)

  override def get(path: String): Option[YamlValue] = Option(config.get(path)).flatMap(Yaml.readObj)

  override def getSection(path: String): Option[ConfigurationSection] = Option(config.getSection(path)).map(new BungeeConfiguration(configPath, _))

  override def getKeys: Iterable[String] = config.getKeys.asScala

  override def set(path: String, yaml: YamlValue): Unit = config.set(path, yaml.value)
}*/
