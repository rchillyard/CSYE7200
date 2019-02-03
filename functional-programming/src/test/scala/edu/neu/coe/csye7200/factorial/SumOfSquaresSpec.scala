package edu.neu.coe.csye7200.factorial

import org.scalatest.{FlatSpec, Matchers}

class SumOfSquaresSpec extends FlatSpec with Matchers {

  behavior of "sumOfSquares"
  it should "work for 1..N" in {
    val n = 1000000
    SumOfSquares.sumOfSquares(n) shouldBe BigInt(n) * (n + 1) * (2 * n + 1) / 6
  }
}
