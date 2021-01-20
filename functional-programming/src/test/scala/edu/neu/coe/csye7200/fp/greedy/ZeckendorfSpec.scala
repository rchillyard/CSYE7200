/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.fp.greedy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ZeckendorfSpec extends AnyFlatSpec with Matchers {

  behavior of "Zeckendorf"

  it should "work for 0" in {
    val target = Zeckendorf
    target(0) shouldBe Nil
  }

  it should "work for 1" in {
    val target = Zeckendorf
    target(1) shouldBe Seq(1)
  }

  it should "work for 2" in {
    val target = Zeckendorf
    target(2) shouldBe Seq(2)
  }

  it should "work for 3" in {
    val target = Zeckendorf
    target(3) shouldBe Seq(3)
  }

  it should "work for 6" in {
    val target = Zeckendorf
    target(6) shouldBe Seq(1, 5)
  }

  it should "work for 10" in {
    val target = Zeckendorf
    target(10) shouldBe Seq(2, 8)
  }

  it should "work for 100" in {
    val target = Zeckendorf
    target(100) shouldBe Seq(3, 8, 89)
  }

  it should "work for 1000" in {
    val target = Zeckendorf
    target(1000) shouldBe Seq(13, 987)
  }

  it should "work for 10000" in {
    val target = Zeckendorf
    target(10000) shouldBe Seq(2, 5, 34, 610, 2584, 6765)
  }

}
