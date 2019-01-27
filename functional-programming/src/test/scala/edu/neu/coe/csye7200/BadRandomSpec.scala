package edu.neu.coe.csye7200

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

/**
  * @author scalaprof
  */
class BadRandomSpec extends FlatSpec with Matchers {
  private val random = Random
  random.setSeed(0L)
  "random" should "be predictable" in {
    random.nextInt
    random.nextInt shouldBe (-723955400)
  }
  it should "be order-independent" in {
    random.nextInt shouldBe 1033096058
  }
}
