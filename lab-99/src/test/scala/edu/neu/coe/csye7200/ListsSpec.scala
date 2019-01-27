/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200

import org.scalatest.{FlatSpec, Matchers}

class ListsSpec extends FlatSpec with Matchers {

  private val fib0 = List(1)
  private val fib5 = List(1, 1, 2, 3, 5, 8)

  behavior of "P01"
  it should "throw an exception for Nil" in {
    a [NoSuchElementException] should be thrownBy P01.last(Nil)
  }
  it should "get 1 for fib0" in {
    P01.last(fib0) shouldBe 1
  }
  it should "get 8 for fib5" in {
    P01.last(fib5) shouldBe 8
  }

  behavior of "P02"
  it should "throw an exception for fib0" in {
    a [NoSuchElementException] should be thrownBy P02.penultimate(fib0)
  }
  it should "get 5 for fib5" in {
    P02.penultimate(fib5) shouldBe 5
  }

  behavior of "P03"
  it should "throw an exception for 0, Nil" in {
    a [NoSuchElementException] should be thrownBy P03.kth(0, Nil)
  }
  it should "throw an exception for 1, fib0" in {
    a [NoSuchElementException] should be thrownBy P03.kth(1, fib0)
  }
  it should "get 3 for 3, fib5" in {
    P03.kth(3, fib5) shouldBe 3
  }

  behavior of "P04"
  it should "get 0 for Nil" in {
    P04.length(Nil) shouldBe 0
  }
  it should "get 6 for fib5" in {
    P04.length(fib5) shouldBe 6
  }

  behavior of "P05"
  it should "reverse Nil to Nil" in {
    P05.reverse(Nil) shouldBe Nil
  }
  it should "reverse singleton list to itself" in {
    P05.reverse(List(1)) shouldBe List(1)
  }
  it should "reverse fib5 correctly" in {
    P05.reverse(fib5) shouldBe List(8, 5, 3, 2, 1, 1)
  }

  behavior of "P06"
  it should "be true for Nil" in {
    P06.isPalindrome(Nil) shouldBe true
  }
  it should "be true for fib0" in {
    P06.isPalindrome(fib0) shouldBe true
  }
  it should "be true for palindrome" in {
    P06.isPalindrome(List(1, 2, 3, 2, 1)) shouldBe true
  }
  it should "be false for fib5" in {
    P06.isPalindrome(fib5) shouldBe false
  }

  behavior of "P07"
  it should "be Nil for Nil" in {
    P07.flatten(Nil) shouldBe Nil
  }
  it should "be List(Nil) for Nil" in {
    P07.flatten(List(Nil)) shouldBe Nil
  }
  it should "be fib5 for components" in {
    P07.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) shouldBe fib5
  }

  behavior of "P08"
  it should "be Nil for Nil" in {
    P08.compress(Nil) shouldBe Nil
  }
  it should "be fib0 for fib0" in {
    P08.compress(fib0) shouldBe fib0
  }
  it should "be fib5.tail for fib5" in {
    P08.compress(fib5) shouldBe fib5.tail
  }
  it should "be unique elements for duplicate list" in {
    P08.compress(List(10, 11, 11, 12, 13, 13, 13, 14, 15, 15, 9)) shouldBe List(10, 11, 12, 13, 14, 15, 9)
  }

  behavior of "P09"
  it should "be Nil for Nil" in {
    P09.pack(Nil) shouldBe Nil
  }
  it should "be fib0 for fib0" in {
    P09.pack(fib0) shouldBe List(fib0)
  }
  it should "be fib5.tail for fib5" in {
    P09.pack(fib5) shouldBe List(List(1,1), List(2), List(3), List(5), List(8))
  }
  it should "be unique elements for duplicate list" in {
    P09.pack(List(10, 11, 11, 12, 13, 13, 13, 14, 15, 15, 9)) shouldBe
      List(List(10), List(11, 11), List(12), List(13,13,13), List(14), List(15,15), List(9))
  }

  behavior of "P10"
  it should "be Nil for Nil" in {
    P10.encode(Nil) shouldBe Nil
  }
  it should "be ((1,1)) for fib0" in {
    P10.encode(fib0) shouldBe List(1 -> 1)
  }
  it should "be correct for fib5" in {
    P10.encode(fib5) shouldBe List(2 -> 1, 1 -> 2, 1 -> 3, 1 -> 5, 1 -> 8)
  }
  it should "be unique elements for duplicate list" in {
    P10.encode(List(10, 11, 11, 12, 13, 13, 13, 14, 15, 15, 9)) shouldBe
      List(1 -> 10, 2 -> 11, 1 -> 12, 3 -> 13, 1 -> 14, 2 -> 15, 1 -> 9)
  }

  behavior of "P11"
  it should "be Nil for Nil" in {
    P11.encodeModified(Nil) shouldBe Nil
  }
  it should "be ((1,1)) for fib0" in {
    P11.encodeModified(fib0) shouldBe List(1)
  }
  it should "be correct for fib5" in {
    P11.encodeModified(fib5) shouldBe List(2 -> 1, 2, 3, 5, 8)
  }
  it should "be unique elements for duplicate list" in {
    P11.encodeModified(List(10, 11, 11, 12, 13, 13, 13, 14, 15, 15, 9)) shouldBe
      List(10, 2 -> 11, 12, 3 -> 13, 14, 2 -> 15, 9)
  }

  behavior of "P12"
  it should "be Nil for Nil" in {
    P12.decode(Nil) shouldBe Nil
  }
  it should "be ((1,1)) for fib0" in {
    P12.decode(List(1 -> 1)) shouldBe fib0
  }
  it should "be correct for fib5" in {
    P12.decode(List(2 -> 1, 1 -> 2, 1 -> 3, 1 -> 5, 1 -> 8)) shouldBe fib5
  }
  it should "be unique elements for duplicate list" in {
    P12.decode(List(1 -> 10, 2 -> 11, 1 -> 12, 3 -> 13, 1 -> 14, 2 -> 15, 1 -> 9)) shouldBe
      List(10, 11, 11, 12, 13, 13, 13, 14, 15, 15, 9)
  }

  behavior of "P13"
  it should "be Nil for Nil" in {
    P13.encodeDirect(Nil) shouldBe Nil
  }
  it should "be ((1,1)) for fib0" in {
    P13.encodeDirect(fib0) shouldBe List(1 -> 1)
  }
  it should "be correct for fib5" in {
    P13.encodeDirect(fib5) shouldBe List(2 -> 1, 1 -> 2, 1 -> 3, 1 -> 5, 1 -> 8)
  }
  it should "be unique elements for duplicate list" in {
    P13.encodeDirect(List(10, 11, 11, 12, 13, 13, 13, 14, 15, 15, 9)) shouldBe
      List(1 -> 10, 2 -> 11, 1 -> 12, 3 -> 13, 1 -> 14, 2 -> 15, 1 -> 9)
  }

  behavior of "P14"
  it should "be Nil for Nil" in {
    P14.duplicate(Nil) shouldBe Nil
  }
  it should "be (1,1) for fib0" in {
    P14.duplicate(fib0) shouldBe List(1, 1)
  }
  it should "be correct for fib5" in {
    P14.duplicate(fib5) shouldBe List(1, 1, 1, 1, 2, 2, 3, 3, 5, 5, 8, 8)
  }

  behavior of "P15"
  it should "be Nil for Nil" in {
    P15.duplicateN(0,Nil) shouldBe Nil
  }
  it should "be Nil for 0, fib0" in {
    P15.duplicateN(0,fib0) shouldBe Nil
  }
  it should "be fib0 for 1, fib0" in {
    P15.duplicateN(1,fib0) shouldBe fib0
  }
  it should "be (1,1) for 2,fib0" in {
    P15.duplicateN(2,fib0) shouldBe List(1, 1)
  }
  it should "be correct for fib5" in {
    P15.duplicateN(3,fib5) shouldBe List(1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 5, 5, 5, 8, 8, 8)
  }

}
