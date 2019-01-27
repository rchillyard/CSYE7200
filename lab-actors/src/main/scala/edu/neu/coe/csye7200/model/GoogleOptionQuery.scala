package edu.neu.coe.csye7200.model

import edu.neu.coe.csye7200.http.UriGet
import spray.http.Uri


/**
  * @author robinhillyard
  */

case class GoogleOptionQuery() extends Query {
  val uriGet = new UriGet()

  def createQuery(symbols: List[String]): Uri = {
    val symbolList = symbols mkString ","
    val queryParams = Map("q" -> s"$symbolList", "output" -> "json")
    uriGet.get(GoogleOptionQuery.server, GoogleOptionQuery.path, queryParams)
  }

  def getProtocol = "json:GO"

}

object GoogleOptionQuery {
  val server = "www.google.com"
  val path = "/finance/option_chain"
}

class GoogleOptionModel extends Model {
  def isOption = true

  def getKey(query: String): Option[String] = query match {
    case "name" => Some("GO")
    case "identifier" => Some("s")
    case "strikePrice" => Some("strike")
    case "expiry" => Some("expiry")
    case "underlying" => Some("underlying_id")
    case "basePrice" => Some("underlying_price")
    case "sharpeRatio" => Some("Sharpe")
    case _ => None
  }
}
