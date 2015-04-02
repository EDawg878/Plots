package com.edawg878.common

import akka.actor.Actor
import akka.contrib.pattern.DistributedPubSubExtension

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
class Publisher extends Actor {

  import akka.contrib.pattern.DistributedPubSubMediator.Publish
  // activate the extension
  val mediator = DistributedPubSubExtension(context.system).mediator

  def receive = {
    case in: String ⇒
      val out = in.toUpperCase
      mediator ! Publish("content", out)
  }
}