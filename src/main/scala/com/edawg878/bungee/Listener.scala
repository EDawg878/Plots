package com.edawg878.bungee

import net.md_5.bungee.api.connection.ProxiedPlayer
import net.md_5.bungee.api.event.ChatEvent
import net.md_5.bungee.api.plugin.{Listener => BungeeListener}
import net.md_5.bungee.event.EventHandler

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Listener {

  class Chat extends BungeeListener {

    @EventHandler
    def onChat(e: ChatEvent) {
        e.getSender match {
          case p: ProxiedPlayer =>

          case _ =>
        }

    }

  }


}
