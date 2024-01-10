package edu.neu.coe.csye7200.asstll

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class RationalSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Rational"

  it should "+" in {
    Rational(1, 3) + Rational(2, 5) shouldBe Rational(11, 15)
    Rational(1, 3) + Rational(1) shouldBe Rational(4, 3)
  }
  it should "*" in {
    Rational(2, 3) * Rational(3, 2) shouldBe Rational(1)
  }
  it should "toString" in {
    Rational(11, 15).toString shouldBe "11/15"
  }


}