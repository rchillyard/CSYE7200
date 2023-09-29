/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.fp.greedy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LucasSpec extends AnyFlatSpec with Matchers {

  behavior of "Lucas"

  it should "yield correct results for small n" in {
    Lucas(0) shouldBe 2L
    Lucas(1) shouldBe 1L
    Lucas(2) shouldBe 3L
    Lucas(3) shouldBe 4L
    Lucas(4) shouldBe 7L
    Lucas(5) shouldBe 11L
    Lucas(6) shouldBe 18L
  }

  it should "implement L38" in {
    Lucas(38) shouldBe 87403803L
  }

  it should "yield correct result for L90" in {
    Lucas(90) shouldBe 6440026026380244498L
  }
}