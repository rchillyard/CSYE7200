package edu.neu.coe.csye7200.fp

import edu.neu.coe.csye7200.FunctionalProgramming
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * @author scalaprof
  */
class FunctionalProgrammingSpec extends AnyFlatSpec with Matchers {

  "doublePrecision" should "not work properly" in {
    val x = FunctionalProgramming.evaluate_3_tenths
    val y = FunctionalProgramming.multiply_by_10_over_3(x)
    y should not be 1
  }


}
