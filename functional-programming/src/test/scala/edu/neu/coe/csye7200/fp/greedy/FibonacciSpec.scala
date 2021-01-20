/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.fp.greedy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FibonacciSpec extends AnyFlatSpec with Matchers {

  behavior of "stream"

  it should "yield correct results from stream" in {
    val target = Fibonacci
    val stream = target.fibonacci
    stream.head shouldBe 0L
    stream(1) shouldBe 1L
    stream(2) shouldBe 1L
    stream(3) shouldBe 2L
    stream(4) shouldBe 3L
    stream(5) shouldBe 5L
    stream(6) shouldBe 8L
  }

  it should "implement getLargest" in {
    val target = Fibonacci
    target.getLargest(10) shouldBe 8L
    target.getLargest(13) shouldBe 13L
  }
}
