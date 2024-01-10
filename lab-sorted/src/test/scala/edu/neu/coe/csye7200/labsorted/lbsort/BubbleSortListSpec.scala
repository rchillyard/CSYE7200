package edu.neu.coe.csye7200.labsorted.lbsort

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class BubbleSortListSpec extends AnyFlatSpec with should.Matchers {

  private val easyAs123: List[Int] = List(1, 2, 3)
  private val reversed: List[Int] = List(3, 2, 1)

  behavior of "BubbleSortList"

  it should "sortList" in {
    val intSorter = BubbleSortList[Int]
    val sorted = intSorter.sort(reversed)
    sorted shouldBe List(1, 2, 3)
  }

  behavior of "sort"

  it should "sort0" in {
    BubbleSortList.sort[Int](Nil) shouldBe Nil
  }

  it should "sort1" in {
    val target: List[Int] = List(1)
    BubbleSortList.sort(target) shouldBe target
  }

  it should "sort2a" in {
    val target: List[Int] = List(1, 2)
    BubbleSortList.sort(target) shouldBe target
  }

  it should "sort2b" in {
    BubbleSortList.sort(List(2, 1)) shouldBe List(1, 2)
  }

  it should "sort3a" in {
    BubbleSortList.sort(easyAs123) shouldBe easyAs123
  }

  it should "sort3b" in {
    BubbleSortList.sort(List(2, 1, 3)) shouldBe easyAs123
  }

  it should "sort3c" in {
    BubbleSortList.sort(reversed) shouldBe easyAs123
  }


}