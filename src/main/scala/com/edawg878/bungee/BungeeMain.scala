package com.edawg878.bungee

import akka.actor.{Props, ActorSystem}
import akka.cluster.Cluster
import com.edawg878.common.Modules.BungeeModule
import com.edawg878.common.{Publisher, Subscriber}
import net.md_5.bungee.api.plugin.Plugin

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
class BungeeMain extends Plugin {

  val bungeeModule = new BungeeModule {
    override def bungeePlugin = BungeeMain.this
  }

  import bungeeModule._

  override def onLoad() {
    val systemName = "Test"
    val system = ActorSystem(systemName)
    val joinAddress = Cluster(system).selfAddress
    Cluster(system).join(joinAddress)
    system.actorOf(Props[Subscriber], "subscriber1")
    val publisher = system.actorOf(Props[Publisher], "publisher1")
    for (i <- 1 to 100) {
      publisher ! "hello"
    }
  }

  override def onEnable() {

  }

}