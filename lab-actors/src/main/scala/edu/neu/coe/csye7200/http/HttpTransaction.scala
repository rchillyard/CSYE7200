package edu.neu.coe.csye7200.http

import akka.actor.{ActorRef, ActorSystem}
import edu.neu.coe.csye7200.actors.HttpResult
import spray.client.pipelining._
import spray.http._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._

/**
  * CONSIDER making this an Actor
  *
  * @author robinhillyard
  */
case class HttpTransaction(queryProtocol: String, request: HttpRequest, actor: ActorRef) {

  import akka.pattern.pipe

  implicit val system: ActorSystem = ActorSystem()

  val pipeline: HttpRequest => Future[HttpResponse] = sendReceive

  val response: Future[HttpResponse] = pipeline(request)

  response map { x => HttpResult(queryProtocol, request, x) } pipeTo actor

}
