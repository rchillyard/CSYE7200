package edu.neu.coe.csye7200.actors

import akka.actor.{Actor, ActorLogging, ActorRef, Props}

/**
  * @author robinhillyard
  *
  */
class Blackboard(forwardMap: Map[Class[_ <: Any], String], actors: Map[String, Class[_ <: BlackboardActor]]) extends Actor with ActorLogging {

  val actorMap: Map[String, ActorRef] = actors map {
    case (k, v) => k -> context.actorOf(Props.create(v, self), k)
  }

  // To encode specific, non-forwarding behavior, override this method
  override def receive: PartialFunction[Any, Unit] = {
    case message =>
      forwardMap.get(message.getClass) match {
        case Some(s) => actorMap.get(s) match {
          case Some(k) => k forward message
          case _ => log.warning(s"no actor established for key $s")
        }
        case _ => log.warning(s"no forward mapping established for message class ${message.getClass}")
      }
  }
}
