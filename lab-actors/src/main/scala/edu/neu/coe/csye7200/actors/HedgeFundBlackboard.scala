package edu.neu.coe.csye7200.actors

import edu.neu.coe.csye7200.http.HttpTransaction
import edu.neu.coe.csye7200.model.Model
import edu.neu.coe.csye7200.portfolio.Portfolio
import spray.client.pipelining._
import spray.http._

/**
  * @author robinhillyard
  *
  */
class HedgeFundBlackboard extends Blackboard(
  Map(classOf[HttpResult] -> "httpReader",
    classOf[KnowledgeUpdate] -> "marketData",
    classOf[SymbolQuery] -> "marketData",
    classOf[OptionQuery] -> "marketData",
    classOf[CandidateOption] -> "optionAnalyzer",
    classOf[PortfolioUpdate] -> "updateLogger",
    classOf[Confirmation] -> "updateLogger"),
  Map("httpReader" -> classOf[HttpReader],
    "marketData" -> classOf[MarketData],
    "optionAnalyzer" -> classOf[OptionAnalyzer],
    "updateLogger" -> classOf[UpdateLogger])) {

  override def receive: PartialFunction[Any, Unit] = {
    case ExternalLookup(protocol, url) =>
      log.debug(s"External lookup with protocol: $protocol and url: $url")
      HttpTransaction(protocol, Get(url), self)
    case m => super.receive(m)
  }
}

case class HttpResult(queryProtocol: String, request: HttpRequest, response: HttpResponse)

case class KnowledgeUpdate(model: Model, symbol: String, update: Map[String, String])

case class CandidateOption(model: Model, identifier: String, put: Boolean, optionDetails: Map[String, String], chainDetails: Map[String, Any])

case class Confirmation(identifier: String, model: Model, attributes: Map[String, Any])

case class SymbolQuery(identifier: String, keys: List[String])

case class OptionQuery(key: String, value: Any)

case class QueryResponse(identifier: String, attributes: Map[String, String])

case class ExternalLookup(queryProtocol: String, url: Uri)

case class PortfolioUpdate(portfolio: Portfolio)
