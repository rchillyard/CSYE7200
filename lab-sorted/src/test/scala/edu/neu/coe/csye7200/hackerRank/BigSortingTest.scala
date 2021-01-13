package edu.neu.coe.csye7200.hackerRank

import edu.neu.coe.csye7200.hackerRank.BigSorting._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BigSortingTest extends AnyFlatSpec with Matchers {

  behavior of "BigSorting"

  it should "do simple comparison" in {
    less("5", "10", 1) shouldBe true
    less("5", "10", 0) shouldBe false
  }
  it should "get less for equal-length strings" in {
    less("123", "123", 0) shouldBe false
    less("123", "123", 1) shouldBe false
    less("123", "123", 2) shouldBe false
    less("234", "123", 0) shouldBe false
    less("234", "123", 1) shouldBe false
    less("234", "123", 2) shouldBe false
    less("123", "234", 0) shouldBe true
    less("123", "234", 1) shouldBe true
    less("123", "234", 2) shouldBe true
  }
  it should "get less for unequal length strings" in {
    less("123", "12", 0) shouldBe false
    less("123", "12", 1) shouldBe false
    less("123", "12", 2) shouldBe false
    less("234", "12", 0) shouldBe false
    less("234", "12", 1) shouldBe false
    less("234", "12", 2) shouldBe false
    less("23", "123", 0) shouldBe false
    less("23", "123", 1) shouldBe false
    less("23", "123", 2) shouldBe true
  }

  it should "swap correctly" in {
    val array = Array("123", "234")
    swap(array, 0, 0)
    array(0) shouldBe "123"
    array(1) shouldBe "234"
    swap(array, 0, 1)
    array(0) shouldBe "234"
    array(1) shouldBe "123"
    swap(array, 1, 0)
    array(0) shouldBe "123"
    array(1) shouldBe "234"
  }

  it should "implement insertionSort correctly" in {
    val array = Array("5", "1", "3", "0", "3", "5")
    insertionSort(array, 0)
    array shouldBe Array("0", "1", "3", "3", "5", "5")
  }

  it should "run insertionSort on array correctly with index 0" in {
    val array = Array("31415926535897932384626433832795", "1", "3", "10", "3", "5")
    insertionSort(array, 0)
    array shouldBe Array("10", "1", "3", "3", "31415926535897932384626433832795", "5")
  }

  it should "run insertionSort multiple times correctly 0" in {
    val array = Array("10", "5")
    insertionSort(array, 0)
    array(0) shouldBe "10"
    array(1) shouldBe "5"
    insertionSort(array, 1)
    array(0) shouldBe "5"
    array(1) shouldBe "10"
  }

  it should "run bigSorting correctly 0" in {
    val array = Array("5", "10")
    bigSorting(array) shouldBe Array("5", "10")
  }

  it should "run bigSorting correctly 1" in {
    val array = Array("123", "234", "12")
    bigSorting(array) shouldBe Array("12", "123", "234")
  }

  it should "run bigSorting correctly 2" in {
    val array = Array("31415926535897932384626433832795", "1", "3", "10", "3", "5")
    bigSorting(array) shouldBe Array("1", "3", "3", "5", "10", "31415926535897932384626433832795")
  }

  it should "digit" in {
    digit("", -1) shouldBe None
    digit("", 0) shouldBe None
    digit("", 1) shouldBe None
    digit("x", 0) shouldBe Some('x')
    digit("x", 1) shouldBe None
    digit("xy", 0) shouldBe Some('y')
    digit("xy", 1) shouldBe Some('x')
    digit("xy", 2) shouldBe None
  }

}
