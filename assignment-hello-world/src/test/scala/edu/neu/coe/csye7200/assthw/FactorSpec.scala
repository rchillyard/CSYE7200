package edu.neu.coe.csye7200.assthw

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class FactorSpec extends AnyFlatSpec with should.Matchers {

    behavior of "Factor"

    it should "isMultiple" in {
        Factor(3).isMultiple(12) shouldBe true
        Factor(4).isMultiple(12) shouldBe true
        Factor(5).isMultiple(12) shouldBe false
    }

    it should "unapply" in {
        Factor(3).unapply(12) shouldBe Some(4)
        Factor(5).unapply(15) shouldBe Some(3)
        Factor(15).unapply(15) shouldBe Some(1)
    }

}