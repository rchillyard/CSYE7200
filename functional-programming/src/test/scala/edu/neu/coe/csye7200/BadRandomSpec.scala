package edu.neu.coe.csye7200

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

/**
  * @author scalaprof
  */
class BadRandomSpec extends AnyFlatSpec with Matchers {
  private val random = Random
  random.setSeed(0L)
  "random" should "be predictable" in {
    random.nextInt()
    random.nextInt() shouldBe (-723955400)
  }
  it should "be order-independent" in {
    random.nextInt() shouldBe 1033096058
  }
}