package com.edawg878.common

import java.io.{IOException, InputStream}
import java.nio.file.{Files, Path}
import java.util.logging.Logger

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
trait Plugin {

  def getDataFolder: Path

  def resolveFile(name: String) = getDataFolder.resolve(name)

  @throws(classOf[IOException])
  def saveResource(in: InputStream, out: String): Unit = Files.copy(in, resolveFile(out))

  def getResource(name: String): InputStream

  def getLogger: Logger

}
