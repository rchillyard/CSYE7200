/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.fp.greedy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PellBadSpec extends AnyFlatSpec with Matchers {

  behavior of "PellBad"

  it should "yield correct results for small n" in {
    new PellBad().get(0) shouldBe 0L
    new PellBad().get(1) shouldBe 1L
    new PellBad().get(2) shouldBe 2L
    new PellBad().get(3) shouldBe 5L
    new PellBad().get(4) shouldBe 12L
    new PellBad().get(5) shouldBe 29L
    new PellBad().get(6) shouldBe 70L
  }

  it should "implement P38" in {
    new PellBad().get(38) shouldBe 124145519261542L
  }

  it should "yield correct result for P90" in {
    new PellBad().get(90) shouldBe 7052354271195710746L
  }

}