package edu.neu.coe.csye7200.fp

import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random

/**
  * I don't recall why this was called BadRandomSpec.
  * But, it seems to work fine now that I have reseeded random before each test.
  *
  * @author scalaprof
  */
class BadRandomSpec extends AnyFlatSpec with Matchers with BeforeAndAfter {
  private val random = Random

  before {
    random.setSeed(0L)
  }

  "random" should "be predictable" in {
    random.nextInt()
    random.nextInt() shouldBe (-723955400)
  }
  it should "be order-independent" in {
    random.nextInt() shouldBe -1155484576
  }
}
