package com.edawg878.common

import akka.actor.{Actor, ActorLogging}
import akka.contrib.pattern.DistributedPubSubExtension

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
class Subscriber extends Actor with ActorLogging {
  import akka.contrib.pattern.DistributedPubSubMediator.Subscribe
  val mediator = DistributedPubSubExtension(context.system).mediator
  // subscribe to the topic named "content"
  mediator ! Subscribe("content", self)

  def receive = {
    case s: String => log.info("Got {}", s)
  }
}