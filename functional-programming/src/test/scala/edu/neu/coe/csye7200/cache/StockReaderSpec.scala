package edu.neu.coe.csye7200.cache

import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success}

class StockReaderSpec extends FlatSpec with Matchers with Futures with ScalaFutures {

  implicit val resource: String = "stocks.txt"

  behavior of "getPrice"

  it should "work" in {
    val xf = StockReader.getPrice("AAPL")
    whenReady(xf) { x => x should matchPattern { case 207.48 => } }
  }

  import scala.concurrent.ExecutionContext.Implicits.global

  it should "fail with badStocks1" in {
    StockReader.getPrice("AAPL")("badStocks1.txt") onComplete {
      case Success(_) => fail("this should fail")
      case Failure(x) => x.getLocalizedMessage should matchPattern { case "no entry matching AAPL" => }
    }
  }
  it should "fail with badStocks2" in {
    StockReader.getPrice("AAPL")("badStocks2.txt") onComplete {
      case Success(_) => fail("this should fail")
      case Failure(x) => x.getLocalizedMessage should matchPattern { case "no entry matching AAPL" => }
    }
  }
  it should "fail with badStocks3" in {
    StockReader.getPrice("AAPL")("badStocks3.txt") onComplete {
      case Success(_) => fail("this should fail")
      case Failure(x) => x should matchPattern { case _: NumberFormatException => }
    }
  }
}
