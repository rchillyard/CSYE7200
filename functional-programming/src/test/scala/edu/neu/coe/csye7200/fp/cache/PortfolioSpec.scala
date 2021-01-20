/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.fp.cache

import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.Future
import scala.util._

class PortfolioSpec extends AnyFlatSpec with Matchers with Futures with ScalaFutures {

  behavior of "Position"

  it should "parse MSFT 55.67" in {
    Position.parse("MSFT 55.67") should matchPattern { case Success(Position("MSFT", 55.67)) => }
  }

  it should "get value" in {
    val xf = Position.value(cache)("MSFT 100.0")
    whenReady(xf) { u => u should matchPattern { case _: Double => } }
    xf.value.get.get shouldBe 10616.0
  }

  behavior of "Portfolio"

  it should "parse MSFT 55.67" in {
    Position.parse("MSFT 55.67") should matchPattern { case Success(Position("MSFT", 55.67)) => }
  }

  it should "parse a portfolio (0)" in {
    val positions = Seq()
    val py: Try[Portfolio] = Portfolio.parse(positions)
    py should matchPattern { case Success(_) => }
    val portfolio = py.get
    val xf: Future[Double] = portfolio.value(cache)
    whenReady(xf) { u => u should matchPattern { case _: Double => } }
    xf.value.get.get shouldBe 0.0
  }

  it should "parse a portfolio (1)" in {
    val positions = Seq("MSFT 100.0")
    val py: Try[Portfolio] = Portfolio.parse(positions)
    py should matchPattern { case Success(_) => }
    val portfolio = py.get
    val xf: Future[Double] = portfolio.value(cache)
    whenReady(xf) { u => u should matchPattern { case _: Double => } }
    xf.value.get.get shouldBe 10616.0
  }

  it should "parse a portfolio (2)" in {
    val positions = Seq("MSFT 55.67", "GOOG 43.20")
    val py: Try[Portfolio] = Portfolio.parse(positions)
    py should matchPattern { case Success(_) => }
    val portfolio = py.get
    val xf: Future[Double] = portfolio.value(cache)
    whenReady(xf) { u => u should matchPattern { case _: Double => } }
    xf.value.get.get shouldBe 51606.4552
  }

  implicit val resource: String = "stocks.txt"
  private val cache = MyCache[String, Double](StockReader.getPrice)

}
