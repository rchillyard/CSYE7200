package edu.neu.coe.csye7200

import org.scalatest.{FlatSpec, Matchers}

/**
  * @author scalaprof
  */
class ProductHelperSpec extends FlatSpec with Matchers {

  case class MockProduct1(x: Int)

  case class MockProduct2(x: Int, y: Int)

  import ProductHelper._

  behavior of "_hashCode"
  it should "work" in {
    MockProduct1(1)._hashCode shouldBe 32
    MockProduct2(1, 2)._hashCode shouldBe 994
  }

  behavior of "_equals"
  it should "work" in {
    MockProduct1(1)._equals(MockProduct1(1)) shouldBe true
    MockProduct2(1, 2)._equals(MockProduct2(1, 2)) shouldBe true
    MockProduct1(1)._equals(MockProduct2(1, 2)) shouldBe false
  }
}
