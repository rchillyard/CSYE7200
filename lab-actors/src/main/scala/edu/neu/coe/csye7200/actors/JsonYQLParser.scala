package edu.neu.coe.csye7200.actors

import akka.actor.ActorRef
import edu.neu.coe.csye7200.model.{Model, YQLModel}
import spray.http._

import scala.util._

/**
  * TODO create a super-type for this kind of actor
  *
  * @author robinhillyard
  */
class JsonYQLParser(blackboard: ActorRef) extends BlackboardActor(blackboard) {

  val model: Model = new YQLModel

  override def receive: PartialFunction[Any, Unit] = {
    case ContentMessage(entity) =>
      log.info("JsonYQLParser received ContentMessage")
      JsonYQLParser.decode(entity) match {
        case Right(response) => processQuote(response.query.results.quote)
        case Left(message) => log.warning(message.toString)
      }
    case m => super.receive(m)
  }

  def processQuote(quotes: Seq[Map[String, Option[String]]]): Unit = quotes foreach { q => processInstrument(q) }

  def processInstrument(quote: Map[String, Option[String]]): Unit = model.getKey("symbol") match {
    case Some(s) =>
      quote.get(s) match {
        case Some(Some(symbol)) =>
          updateMarket(symbol, quote)
          log.info(s"JsonYQLParser updated $symbol with price $quote")
        case _ => log.warning(s"symbol $s is undefined")
      }
    case _ => log.warning("'symbol' is undefined in model")
  }

  def updateMarket(symbol: String, quote: Map[String, Option[String]]): Unit = blackboard ! KnowledgeUpdate(model, symbol, quote flatMap { case (k, Some(v)) => Option(k -> v); case _ => None })
}

object JsonYQLParser {

  import spray.httpx.SprayJsonSupport._
  import spray.httpx.unmarshalling._
  import spray.json.{DefaultJsonProtocol, _}

  case class Response(query: Query)

  case class Query(count: Int, created: String, lang: String, diagnostics: Option[Diagnostics], results: Results)

  case class Diagnostics(url: Seq[Map[String, String]], publiclyCallable: String, `user-time`: String, `service-time`: String, `build-version`: String, query: DiagnosticsQuery,
                         cache: DiagnosticsCache, javascript: DiagnosticsJavascript)

  case class DiagnosticsQuery(`execution-start-time`: String, `execution-stop-time`: String, `execution-time`: String, params: String, content: String)

  case class DiagnosticsCache(`execution-start-time`: String, `execution-stop-time`: String, `execution-time`: String, method: String, `type`: String, content: String)

  case class DiagnosticsJavascript(`execution-start-time`: String, `execution-stop-time`: String, `execution-time`: String, `instructions-used`: String, `table-name`: String)

  case class Results(quote: Seq[Map[String, Option[String]]]) {
    def get(index: Int, key: String): Option[String] = {
      Try {
        quote(index)
      } match {
        case Success(y) => y.get(key) match {
          case Some(x) => x;
          case None => None
        }
        case Failure(_) => None
      }
    }
  }

  object MyJsonProtocol extends DefaultJsonProtocol with NullOptions {
    implicit val diagnosticsQueryFormat: RootJsonFormat[DiagnosticsQuery] = jsonFormat5(DiagnosticsQuery)
    implicit val diagnosticsCacheFormat: RootJsonFormat[DiagnosticsCache] = jsonFormat6(DiagnosticsCache)
    implicit val diagnosticsJavascriptFormat: RootJsonFormat[DiagnosticsJavascript] = jsonFormat5(DiagnosticsJavascript)
    implicit val diagnosticsFormat: RootJsonFormat[Diagnostics] = jsonFormat8(Diagnostics)
    implicit val resultsFormat: RootJsonFormat[Results] = jsonFormat1(Results)
    implicit val queryFormat: RootJsonFormat[Query] = jsonFormat5(Query)
    implicit val entityFormat: RootJsonFormat[Response] = jsonFormat1(Response)
  }

  import MyJsonProtocol._

  def decode(entity: HttpEntity): Deserialized[Response] = entity.as[Response]

}
