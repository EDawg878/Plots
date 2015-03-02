package com.edawg878.common

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
trait Console {

  def sendMessage(message: String)

  def sendMessage(messages: String*): Unit = messages.foreach(sendMessage)

  def hasPermission(permission: String): Boolean

}
