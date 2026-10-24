package edu.neu.coe.csye7200.labsorted.lbsort

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.Random

class MergeSortListSpec extends AnyFlatSpec with should.Matchers {

  behavior of "MergeSortList"

  it should "sort an empty list" in {
    MergeSortList.sort[Int](Nil) shouldBe Nil
  }

  it should "sort a singleton" in {
    MergeSortList.sort(List(1)) shouldBe List(1)
  }

  it should "sort a reversed list" in {
    MergeSortList.sort(List(3, 2, 1)) shouldBe List(1, 2, 3)
  }

  it should "keep duplicates" in {
    MergeSortList.sort(List(2, 1, 2, 1)) shouldBe List(1, 1, 2, 2)
  }

  it should "sort Strings" in {
    MergeSortList.sort(List("gamma", "alpha", "beta")) shouldBe List("alpha", "beta", "gamma")
  }

  it should "agree with the standard library on random input" in {
    val random = new Random(0)
    for (n <- Seq(0, 1, 2, 5, 17, 64, 257, 1000)) {
      val xs = List.fill(n)(random.nextInt())
      MergeSortList.sort(xs) shouldBe xs.sorted
    }
  }

  it should "give the same answer at every cutoff" in {
    // The cutoff is a performance knob and must not change the result. Cutoff 1
    // disables the base case altogether, giving plain merge sort.
    val random = new Random(1)
    val xs = List.fill(500)(random.nextInt(50))
    val expected = xs.sorted
    for (cutoff <- Seq(1, 2, 3, 8, 16, 32, 499, 500, 501, 1000))
      withClue(s"cutoff $cutoff: ") {
        MergeSortList.withCutoff[Int](cutoff).sort(xs) shouldBe expected
      }
  }

  it should "be stable" in {
    given Ordering[(Int, String)] = Ordering.by(_._1)

    val xs = List(1 -> "a", 0 -> "b", 1 -> "c", 0 -> "d", 1 -> "e")
    // Small enough to go straight to the base case, and again with merging.
    MergeSortList.sort(xs) shouldBe List(0 -> "b", 0 -> "d", 1 -> "a", 1 -> "c", 1 -> "e")
    MergeSortList.withCutoff[(Int, String)](1).sort(xs) shouldBe
            List(0 -> "b", 0 -> "d", 1 -> "a", 1 -> "c", 1 -> "e")
  }

  it should "honour a substituted base case" in {
    // sortShort is protected so that the cutoff benchmark can swap in another
    // sort; substituting one must not change the answer.
    class WithBubble[X: Ordering](cutoff: Int) extends MergeSortList[X](cutoff) {
      private val bubble = new BubbleSortList[X]

      override protected def sortShort(xs: List[X]): List[X] = bubble.sort(xs)
    }

    val random = new Random(2)
    val xs = List.fill(300)(random.nextInt())
    new WithBubble[Int](16).sort(xs) shouldBe xs.sorted
  }

  it should "not overflow the stack on a long list" in {
    val n = 100000
    val random = new Random(3)
    val xs = List.fill(n)(random.nextInt())
    MergeSortList.sort(xs) shouldBe xs.sorted
  }
}
