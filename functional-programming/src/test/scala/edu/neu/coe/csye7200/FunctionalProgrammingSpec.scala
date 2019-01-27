package edu.neu.coe.csye7200

import org.scalatest.{FlatSpec, Matchers}

/**
  * @author scalaprof
  */
class FunctionalProgrammingSpec extends FlatSpec with Matchers {

  "doublePrecision" should "not work properly" in {
    val x = FunctionalProgramming.evaluate_3_tenths
    val y = FunctionalProgramming.multiply_by_10_over_3(x)
    y should not be 1
  }


}
