package edu.neu.coe.csye7200.labsorted.lbsort

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.Random

class InsertionSortListSpec extends AnyFlatSpec with should.Matchers {

  behavior of "InsertionSortList"

  it should "sort an empty list" in {
    InsertionSortList.sort[Int](Nil) shouldBe Nil
  }

  it should "sort a singleton" in {
    InsertionSortList.sort(List(1)) shouldBe List(1)
  }

  it should "sort a reversed list" in {
    InsertionSortList.sort(List(3, 2, 1)) shouldBe List(1, 2, 3)
  }

  it should "leave an already-sorted list alone" in {
    InsertionSortList.sort(List(1, 2, 3)) shouldBe List(1, 2, 3)
  }

  it should "keep duplicates" in {
    InsertionSortList.sort(List(2, 1, 2, 1)) shouldBe List(1, 1, 2, 2)
  }

  it should "sort Strings" in {
    InsertionSortList.sort(List("gamma", "alpha", "beta")) shouldBe List("alpha", "beta", "gamma")
  }

  it should "agree with the standard library on random input" in {
    val random = new Random(0)
    for (n <- Seq(0, 1, 2, 5, 17, 64, 257)) {
      val xs = List.fill(n)(random.nextInt())
      InsertionSortList.sort(xs) shouldBe xs.sorted
    }
  }

  it should "be stable" in {
    // Ordered on the first element only, so a stable sort must leave pairs with
    // equal keys in their original relative order.
    given Ordering[(Int, String)] = Ordering.by(_._1)

    val xs = List(1 -> "a", 0 -> "b", 1 -> "c", 0 -> "d", 1 -> "e")
    InsertionSortList.sort(xs) shouldBe List(0 -> "b", 0 -> "d", 1 -> "a", 1 -> "c", 1 -> "e")
  }

  it should "give the same answer from sortBounded as from sort" in {
    // sortBounded trades stack safety for half the allocation and is what
    // MergeSortList's base case uses. It must agree with sort exactly.
    val sorter = InsertionSortList[Int]
    val random = new Random(4)
    for (n <- Seq(0, 1, 2, 3, 8, 16, 33, 64)) {
      val xs = List.fill(n)(random.nextInt(20))
      withClue(s"n = $n: ") {
        sorter.sortBounded(xs) shouldBe sorter.sort(xs)
        sorter.sortBounded(xs) shouldBe xs.sorted
      }
    }
  }

  it should "be stable in sortBounded too" in {
    given Ordering[(Int, String)] = Ordering.by(_._1)

    val xs = List(1 -> "a", 0 -> "b", 1 -> "c", 0 -> "d", 1 -> "e")
    InsertionSortList[(Int, String)].sortBounded(xs) shouldBe
            List(0 -> "b", 0 -> "d", 1 -> "a", 1 -> "c", 1 -> "e")
  }

  it should "not overflow the stack on a long list" in {
    // insert is tail-recursive; a reversed input makes every insertion traverse
    // the whole of the sorted prefix, which is the worst case for its depth.
    val n = 20000
    val sorted = InsertionSortList.sort((1 to n).reverse.toList)
    sorted.head shouldBe 1
    sorted.last shouldBe n
  }
}
