package edu.neu.coe.csye7200.labsorted.lbsort

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class BubbleSortSpec extends AnyFlatSpec with should.Matchers {

  private val easyAs123: Seq[Int] = Seq(1, 2, 3)
  private val reversed: Seq[Int] = Seq(3, 2, 1)

  behavior of "BubbleSort"

  it should "sortList" in {
    val intSorter = BubbleSort[Int]()
    Element.create(reversed) match {
      case None =>
        fail("create failed")
      case Some(xe) =>
        intSorter.sortList(xe)
        xe shouldBe Element(3, Some(Element(2, Some(Element(1, None)))))
    }
  }

  behavior of "sort"

  it should "sort0" in {
    BubbleSort.sort[Int](Nil) shouldBe Nil
  }

  it should "sort1" in {
    val target: Seq[Int] = Seq(1)
    BubbleSort.sort(target) shouldBe target
  }

  it should "sort2a" in {
    val target: Seq[Int] = Seq(1, 2)
    BubbleSort.sort(target) shouldBe target
  }

  it should "sort2b" in {
    BubbleSort.sort(Seq(2, 1)) shouldBe Seq(1, 2)
  }

  it should "sort3a" in {
    BubbleSort.sort(easyAs123) shouldBe easyAs123
  }

  it should "sort3b" in {
    BubbleSort.sort(Seq(2, 1, 3)) shouldBe easyAs123
  }

  it should "sort3c" in {
    BubbleSort.sort(reversed) shouldBe easyAs123
  }

  behavior of "Element"

  it should "create Element" in {
    Element.create(Nil) shouldBe None
    Element.create(Seq(0)) shouldBe Some(Element(0, None))
    Element.create(Seq(0, 1)) shouldBe Some(Element(0, Some(Element(1, None))))
  }

}