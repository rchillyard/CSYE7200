package edu.neu.coe.csye7200.actors

import akka.actor.{Actor, ActorLogging, ActorRef}

/**
  * @author robinhillyard
  */
abstract class BlackboardActor(blackboard: ActorRef) extends Actor with ActorLogging {
  override def receive: PartialFunction[Any, Unit] = {
    case m => log.warning("uncaught message type: {}", m)
  }

  //  def props[A <: BlackboardActor](clazz: Class[A]) = Props.create(clazz, blackboard)
}
