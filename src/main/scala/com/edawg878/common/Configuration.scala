package com.edawg878.common

import java.nio.file.Files

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
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
