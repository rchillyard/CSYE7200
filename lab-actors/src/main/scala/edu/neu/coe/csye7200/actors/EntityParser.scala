package edu.neu.coe.csye7200.actors

import akka.actor.{ActorRef, Props}

/**
  * @author robinhillyard
  */
class EntityParser(blackboard: ActorRef) extends BlackboardActor(blackboard) {

  val parsers = Map("json:YQL" -> context.actorOf(Props.create(classOf[JsonYQLParser], blackboard), "JsonParserYQL"),
    "json:GF" -> context.actorOf(Props.create(classOf[JsonGoogleParser], blackboard), "JsonGoogleParser"),
    "json:GO" -> context.actorOf(Props.create(classOf[JsonGoogleOptionParser], blackboard), "JsonGoogleOptionParser"))

  override def receive: PartialFunction[Any, Unit] = {
    case EntityMessage(protocol, entity) =>
      log.debug("EntityMessage received: protocol: {}", protocol)
      parsers.get(protocol) match {
        case Some(actorRef) => actorRef ! ContentMessage(entity)
        case None => log.warning("no parser for: {}", protocol)
      }
    case m => super.receive(m)
  }
}

case class ContentMessage(content: spray.http.HttpEntity)
