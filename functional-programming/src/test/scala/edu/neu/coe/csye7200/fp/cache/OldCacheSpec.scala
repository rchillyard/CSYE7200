/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.fp.cache

import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

class OldCacheSpec extends AnyFlatSpec with Matchers with Futures with ScalaFutures {

  behavior of "apply"

  val random: Random.type = Random

  def lookupStock(k: String): Future[Double] = Future {
    random.setSeed(k.hashCode)
    random.nextInt(1000) / 100.0
  }

  it should "work" in {
    val cache = MyCache[String, Double](lookupStock)
    val xf: Future[Double] = cache("MSFT")
    whenReady(xf) { u => u should matchPattern { case _: Double => } }
    xf.value.get.get shouldBe 3.64
  }
}
