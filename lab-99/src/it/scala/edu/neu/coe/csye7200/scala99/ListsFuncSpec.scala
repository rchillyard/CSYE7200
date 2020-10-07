/*
 * Copyright (c) 2019. Phasmid Software
 */

package edu.neu.coe.csye7200.scala99

import org.scalatest.flatspec
import org.scalatest.matchers.should

import scala.language.postfixOps

class ListsFuncSpec extends flatspec.AnyFlatSpec with should.Matchers {

  val fibonacci: LazyList[Long] = 1L #:: fibonacci.scanLeft(1L)(_ + _)

  behavior of "P01"
  it should "get 817770325994397771L for fib1000" in {
    P01.last(fibonacci take 1000 toList) shouldBe 817770325994397771L
  }

  behavior of "P02"
  it should "get 5 for fib6" in {
    P02.penultimate(fibonacci take 6 toList) shouldBe 5
  }

  behavior of "P03"
  it should "get 3 for 3, fib5" in {
    P03.kth(3, fibonacci take 5 toList) shouldBe 3
  }

  behavior of "P04"
  it should "get 5 for fib5" in {
    P04.length(fibonacci take 5 toList) shouldBe 5
  }

  behavior of "P05"
  it should "reverse fib6 correctly" in {
    P05.reverse(fibonacci take 6 toList) shouldBe List(8, 5, 3, 2, 1, 1)
  }

  behavior of "P06"
  it should "be true for palindrome" in {
    P06.isPalindrome(List(1, 2, 3, 2, 1)) shouldBe true
  }

  behavior of "P07"
  it should "be fib6 for components" in {
    P07.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) shouldBe (fibonacci take 6 toList)
  }

  behavior of "P08"
  it should "be unique elements for duplicate list" in {
    P08.compress(List(10, 11, 11, 12, 13, 13, 13, 14, 15, 15, 9)) shouldBe List(10, 11, 12, 13, 14, 15, 9)
  }

  behavior of "P09"
  it should "be unique elements for duplicate list" in {
    P09.pack(List(10, 11, 11, 12, 13, 13, 13, 14, 15, 15, 9)) shouldBe
      List(List(10), List(11, 11), List(12), List(13,13,13), List(14), List(15,15), List(9))
  }

  behavior of "P10"
  it should "be unique elements for duplicate list" in {
    P10.encode(List(10, 11, 11, 12, 13, 13, 13, 14, 15, 15, 9)) shouldBe
      List(1 -> 10, 2 -> 11, 1 -> 12, 3 -> 13, 1 -> 14, 2 -> 15, 1 -> 9)
  }

  behavior of "P11"
  it should "be unique elements for duplicate list" in {
    P11.encodeModified(List(10, 11, 11, 12, 13, 13, 13, 14, 15, 15, 9)) shouldBe
      List(10, 2 -> 11, 12, 3 -> 13, 14, 2 -> 15, 9)
  }

  behavior of "P12"
  it should "be unique elements for duplicate list" in {
    P12.decode(List(1 -> 10, 2 -> 11, 1 -> 12, 3 -> 13, 1 -> 14, 2 -> 15, 1 -> 9)) shouldBe
      List(10, 11, 11, 12, 13, 13, 13, 14, 15, 15, 9)
  }

  behavior of "P13"
  it should "be unique elements for duplicate list" in {
    P13.encodeDirect(List(10, 11, 11, 12, 13, 13, 13, 14, 15, 15, 9)) shouldBe
      List(1 -> 10, 2 -> 11, 1 -> 12, 3 -> 13, 1 -> 14, 2 -> 15, 1 -> 9)
  }

  behavior of "P14"
  it should "be Nil for Nil" in {
    P14.duplicate(Nil) shouldBe Nil
  }

  behavior of "P15"
  it should "be Nil for Nil" in {
    P15.duplicateN(0,Nil) shouldBe Nil
  }

}
