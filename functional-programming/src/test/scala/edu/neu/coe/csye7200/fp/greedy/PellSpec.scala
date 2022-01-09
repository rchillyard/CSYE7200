/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.fp.greedy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PellSpec extends AnyFlatSpec with Matchers {

  behavior of "Pell"

  it should "yield correct results for small n" in {
    Pell(0) shouldBe 0L
    Pell(1) shouldBe 1L
    Pell(2) shouldBe 2L
    Pell(3) shouldBe 5L
    Pell(4) shouldBe 12L
    Pell(5) shouldBe 29L
    Pell(6) shouldBe 70L
  }

  it should "implement P38" in {
    Pell(38) shouldBe 124145519261542L
  }

  it should "yield correct result for P90" in {
    Pell.bad(90) shouldBe 7052354271195710746L
  }

}
