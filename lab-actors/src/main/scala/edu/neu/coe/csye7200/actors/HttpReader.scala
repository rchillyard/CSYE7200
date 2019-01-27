package edu.neu.coe.csye7200.actors

import akka.actor.{ActorRef, Props}
import spray.http._

/**
  * @author robinhillyard
  */
class HttpReader(blackboard: ActorRef) extends BlackboardActor(blackboard) {

  val entityParser: ActorRef = context.actorOf(Props.create(classOf[EntityParser], blackboard), "EntityParser")

  /**
    * @return
    */
  override def receive: PartialFunction[Any, Unit] = {
    case HttpResult(queryProtocol, request, HttpResponse(status, entity, headers, protocol)) =>
      log.info("request sent: {}; protocol: {}; response status: {}", request, protocol, status)
      if (status.isSuccess)
        processResponse(entity, headers, queryProtocol)
      else
        log.error("HTTP transaction error: {}", status.reason)

    case m => super.receive(m)
  }

  def processResponse(entity: HttpEntity, headers: List[HttpHeader], protocol: String): Unit = {
    log.debug("response headers: {}; entity: {}", headers, entity)
    entityParser ! EntityMessage(protocol, entity)
  }
}

// TODO add headers
// CONSIDER move into Blackboard
case class EntityMessage(protocol: String, entity: HttpEntity)
