/*
 * Copyright (c) 2019. Phasmid Software
 */

package edu.neu.coe.csye7200.scala99

import org.scalatest.flatspec
import org.scalatest.matchers.should

class ArithmeticFuncSpec extends flatspec.AnyFlatSpec with should.Matchers {

  import Arithmetic._

  behavior of "P31"
  it should "get true for 7" in {
    7.isPrime shouldBe true
  }

  behavior of "P32"
  it should "get 9 for 36,63" in {
    gcd(36,63) shouldBe 9
  }

  behavior of "P33"
  it should "get true for 35,64" in {
    35.isCoprimeTo(64) shouldBe true
  }

  behavior of "P34"
  it should "get 4 for 10" in {
    10.totient shouldBe 4
  }

  behavior of "P35"
  it should "get 3,3,5,7 for 315" in {
    315.primeFactors shouldBe List(3, 3, 5, 7)
  }

  behavior of "P36"
  it should "get 3->2, 5->1, 7->1 for 315" in {
    315.primeFactorMultiplicity shouldBe Map(3 -> 2, 5 -> 1, 7 -> 1)
  }

  behavior of "P37"
  it should "get " in {
//    gcd(36,63) shouldBe 9
  }

  behavior of "P38"
  it should "get " in {
//    gcd(36,63) shouldBe 9
  }

  behavior of "P39"
  it should "get 7, 11, 13, 17, 19, 23, 29, 31 for 7,31" in {
    listPrimesInRange(7 to 31) shouldBe List(7, 11, 13, 17, 19, 23, 29, 31)
  }

  behavior of "P40"
  it should "get 5, 23 for 28" in {
    28.goldbach shouldBe (5, 23)
  }

  behavior of "P41"
  it should "get " in {
//    gcd(36,63) shouldBe 9
  }
}

