package edu.neu.coe.csye7200.portfolio

/**
  * CONSIDER moving this into model package
  *
  * @author robinhillyard
  */
case class Portfolio(name: String, positions: Seq[Position])

case class Position(symbol: String, quantity: Int, contracts: Seq[Contract])

case class Contract(id: String)

object PortfolioParser {

  import spray.json.{DefaultJsonProtocol, _}

  object MyJsonProtocol extends DefaultJsonProtocol with NullOptions {
    implicit val contractFormat: RootJsonFormat[Contract] = jsonFormat1(Contract)
    implicit val positionFormat: RootJsonFormat[Position] = jsonFormat3(Position)
    implicit val portfolioFormat: RootJsonFormat[Portfolio] = jsonFormat2(Portfolio)
  }

  import MyJsonProtocol._

  def decode(json: String): Portfolio =
    json.parseJson.convertTo[Portfolio]
}
