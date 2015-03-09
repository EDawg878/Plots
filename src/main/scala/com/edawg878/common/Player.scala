package com.edawg878.common

import java.util.UUID

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
trait Player {

  def sendMessage(message: String)

  def sendMessage(messages: String*): Unit = messages.foreach(sendMessage)

  def hasPermission(permission: String): Boolean

  def getDisplayName: String

  def getName: String

  def getUniqueId: UUID

}
