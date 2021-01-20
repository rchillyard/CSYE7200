/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.cache

import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util._

class PortfolioSpec extends AnyFlatSpec with Matchers with Futures with ScalaFutures {

  private val cache = FulfillingCache[String, Double](lookupStock)

  behavior of "Position"

  it should "parse MSFT 55.67" in {
    Position.parse("MSFT 55.67") should matchPattern { case Success(Position("MSFT", 55.67)) => }
  }

  it should "get value" in {
    val xf = Position.value(cache)("MSFT 100.0")
    whenReady(xf) { u => u should matchPattern { case _: Double => } }
    xf.value.get.get shouldBe 364
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
    xf.value.get.get shouldBe 364
  }

  it should "parse a portfolio (2)" in {
    val positions = Seq("MSFT 55.67", "GOOG 43.20")
    val py: Try[Portfolio] = Portfolio.parse(positions)
    py should matchPattern { case Success(_) => }
    val portfolio = py.get
    val xf: Future[Double] = portfolio.value(cache)
    whenReady(xf) { u => u should matchPattern { case _: Double => } }
    xf.value.get.get shouldBe 345.6308
  }

  private val random = Random

  def lookupStock(k: String): Future[Double] = Future {
    random.setSeed(k.hashCode)
    random.nextInt(1000) / 100.0
  }

}
