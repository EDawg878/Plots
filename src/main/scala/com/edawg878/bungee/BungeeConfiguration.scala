package com.edawg878.bungee

import com.edawg878.common.Server.{Configuration, Plugin}
import net.md_5.bungee.config.{YamlConfiguration, ConfigurationProvider}
import scala.collection.JavaConverters._

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
class BungeeConfiguration(plugin: Plugin, name: String, config: net.md_5.bungee.config.Configuration)
  extends Configuration(plugin, name) {

  val configProvider = ConfigurationProvider.getProvider(classOf[YamlConfiguration])


  def this(plugin: Plugin, name: String) = this(plugin, name, configProvider.load(plugin.resolveFile(name).toFile))

  override def reload: Configuration = new BungeeConfiguration(plugin, name, configProvider.load(path.toFile))

  override def save(): Unit = configProvider.save(config, path.toFile)

  override def get(path: String): AnyRef = config.get(path)

  override def getSection(path: String): Configuration = new BungeeConfiguration(plugin, name)

  override def set(path: String, value: AnyVal): Unit = config.set(path, value)

  override def get[T](path: String, default: T): Unit = config.get(path, default)

  override def getKeys: Iterable[String] = config.getKeys.asScala

}
