package edu.neu.coe.csye7200

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PrimeSpec extends AnyFlatSpec with should.Matchers {

    behavior of ""
    it should "" in {
        Prime.primeFactorMultiplicity(160) shouldBe Map(2 -> 5, 5 -> 1)

        Prime.primeFactorMultiplicity(827) shouldBe Map(827 -> 1)
    }
}