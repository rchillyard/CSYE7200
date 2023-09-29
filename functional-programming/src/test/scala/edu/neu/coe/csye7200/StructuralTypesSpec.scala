package edu.neu.coe.csye7200

import edu.neu.coe.csye7200.StructuralTypes.Large
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StructuralTypesSpec extends AnyFlatSpec with Matchers {

    behavior of "Large"

    it should "toString" in {
        val x = Large(160)
        StructuralTypes.toString(x) shouldBe "Map(2 -> 5, 5 -> 1)"
    }

}