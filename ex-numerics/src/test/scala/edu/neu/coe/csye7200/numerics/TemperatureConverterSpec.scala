package edu.neu.coe.csye7200.numerics

import edu.neu.coe.csye7200.numerics.TemperatureConverter.{cToF, cToFs, fToC, fToCs}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TemperatureConverterSpec extends AnyFlatSpec with should.Matchers {

  behavior of "TemperatureConverterSpec"

  it should "fToCs" in {
    fToCs("-40") shouldBe "-40.0C"
    fToCs("32") shouldBe "0.0C"
    fToCs("50") shouldBe "10.0C"
    fToCs("68") shouldBe "20.0C"
  }

  it should "cToF" in {
    cToF(-40) shouldBe -40.0
    cToF(0) shouldBe 32.0
    cToF(10) shouldBe 50.0
    cToF(20) shouldBe 68.0
  }

  it should "cToFs" in {
    cToFs("-40") shouldBe "-40.0F"
    cToFs("0") shouldBe "32.0F"
    cToFs("10") shouldBe "50.0F"
    cToFs("20") shouldBe "68.0F"
  }

  it should "fToC" in {
    fToC(-40) shouldBe -40.0
    fToC(32) shouldBe 0.0
    fToC(50) shouldBe 10.0
    fToC(68) shouldBe 20.0
  }

}
