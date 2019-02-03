package edu.neu.coe.csye7200.actors

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit._
import edu.neu.coe.csye7200.actors.JsonGoogleParser.Results
import edu.neu.coe.csye7200.model.Model
import org.scalatest._
import spray.http._

import scala.concurrent.duration._
import scala.io.Source
import scala.language.postfixOps

/**
  * This specification really tests much of the HedgeFund app but because it particularly deals with
  * processing data from the YQL (Yahoo Query Language) using JSON, we call it by its given name.
  */
class JsonGoogleParserSpec(_system: ActorSystem) extends TestKit(_system) with ImplicitSender
  with WordSpecLike with Matchers with Inside with BeforeAndAfterAll {

  def this() = this(ActorSystem("JsonGoogleParserSpec"))

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  import scala.language.postfixOps

  val json: String = Source.fromFile(getClass.getResource("/googleExample.json").getPath) mkString

  "json read" in {
    import JsonGoogleParser.MyJsonProtocol._
    import spray.json._
    val obj: Results = JsonGoogleParser.fix(json).parseJson.convertTo[Results]
    obj shouldBe List(Map("e" -> Some("NASDAQ"), "elt" -> Some("Jul 24, 7:15PM EDT"), "s" -> Some("2"), "ec" -> Some("+0.05"), "cp_fix" -> Some("-0.53"), "l_cur" -> Some("124.50"), "ccol" -> Some("chr"), "t" -> Some("AAPL"), "el" -> Some("124.55"), "yld" -> Some("1.67"), "div" -> Some("0.52"), "pcls_fix" -> Some("125.16"), "el_cur" -> Some("124.55"), "id" -> Some("22144"), "ec_fix" -> Some("0.05"), "l" -> Some("124.50"), "el_fix" -> Some("124.55"), "l_fix" -> Some("124.50"), "ecp_fix" -> Some("0.04"), "c_fix" -> Some("-0.66"), "c" -> Some("-0.66"), "eccol" -> Some("chg"), "cp" -> Some("-0.53"), "lt" -> Some("Jul 24, 4:08PM EDT"), "ecp" -> Some("0.04"), "lt_dts" -> Some("2015-07-24T16:08:30Z"), "ltt" -> Some("4:08PM EDT")), Map("e" -> Some("NASDAQ"), "elt" -> Some("Jul 24, 6:34PM EDT"), "s" -> Some("2"), "ec" -> Some("+0.02"), "cp_fix" -> Some("-0.92"), "l_cur" -> Some("38.85"), "ccol" -> Some("chr"), "t" -> Some("YHOO"), "el" -> Some("38.87"), "yld" -> Some(""), "div" -> Some(""), "pcls_fix" -> Some("39.21"), "el_cur" -> Some("38.87"), "id" -> Some("658890"), "ec_fix" -> Some("0.02"), "l" -> Some("38.85"), "el_fix" -> Some("38.87"), "l_fix" -> Some("38.85"), "ecp_fix" -> Some("0.06"), "c_fix" -> Some("-0.36"), "c" -> Some("-0.36"), "eccol" -> Some("chg"), "cp" -> Some("-0.92"), "lt" -> Some("Jul 24, 4:08PM EDT"), "ecp" -> Some("0.06"), "lt_dts" -> Some("2015-07-24T16:08:28Z"), "ltt" -> Some("4:08PM EDT")))
  }

  "json conversion" in {
    val contentTypeText = ContentType(MediaTypes.`text/html`, HttpCharsets.`ISO-8859-1`)
    val entity = HttpEntity(contentTypeText, json.getBytes())
    val ok = JsonGoogleParser.decode(entity) match {
      case Right(x) =>
        val z = x
        x.seq.length should equal(2)
        val quotes = x.seq
        quotes.head.get("t") should matchPattern { case Some(Some("AAPL")) => }

      case Left(x) =>
        fail("decoding error: " + x)
    }
    ok shouldBe Succeeded
  }

  "send back" in {
    val blackboard = system.actorOf(Props.create(classOf[MockGoogleBlackboard], testActor), "blackboard")
    val contentType = ContentType(MediaTypes.`text/html`, HttpCharsets.`ISO-8859-1`)
    val entityParser = _system.actorOf(Props.create(classOf[EntityParser], blackboard), "entityParser")
    val entity = HttpEntity(contentType, json.getBytes())
    entityParser ! EntityMessage("json:GF", entity)
    val msg = expectMsgClass(3.seconds, classOf[QueryResponse])
    println("msg received: " + msg)
    msg should matchPattern {
      case QueryResponse("AAPL", _) =>
    }
    inside(msg) {
      case QueryResponse(_, attributes) => attributes.get("l") should matchPattern { case Some("124.50") => }
    }
  }
}

import akka.pattern.ask

import scala.concurrent.Await

class MockGoogleUpdateLogger(blackboard: ActorRef) extends UpdateLogger(blackboard) {
  override def processStock(identifier: String, model: Model): Unit = {
    model.getKey("price") match {
      case Some(p) =>
        // sender is the MarketData actor
        val future = sender ? SymbolQuery(identifier, List(p))
        val result = Await.result(future, timeout.duration).asInstanceOf[QueryResponse]
        result.attributes foreach {
          case (k, v) =>
            log.info(s"$identifier attribute $k has been updated to: $v")
            blackboard ! result
        }
      case None => log.warning(s"'price' not defined in model")
    }
  }
}

class MockGoogleBlackboard(testActor: ActorRef) extends Blackboard(Map(classOf[KnowledgeUpdate] -> "marketData", classOf[SymbolQuery] -> "marketData", classOf[OptionQuery] -> "marketData", classOf[CandidateOption] -> "optionAnalyzer", classOf[Confirmation] -> "updateLogger"),
  Map("marketData" -> classOf[MarketData], "optionAnalyzer" -> classOf[OptionAnalyzer], "updateLogger" -> classOf[MockGoogleUpdateLogger])) {

  override def receive: PartialFunction[Any, Unit] = {
    case msg: Confirmation => msg match {
      // Cut down on the volume of messages
      case Confirmation("AAPL", _, _) => super.receive(msg)
      case _ =>
    }
    case msg: QueryResponse => testActor forward msg

    case msg => super.receive(msg)
  }
}
