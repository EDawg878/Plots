package com.edawg878.common

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
trait ConfigurationSection {

  def get(path: String): AnyRef

  def get[T](path: String, default: T)

  def set(path: String, value: AnyVal)

  def getSection(path: String): ConfigurationSection

  def getKeys: Iterable[String]

}
