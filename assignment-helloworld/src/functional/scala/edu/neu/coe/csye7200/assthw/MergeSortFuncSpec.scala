package edu.neu.coe.csye7200.assthw

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MergeSortFuncSpec extends AnyFlatSpec with should.Matchers {

  behavior of "MergeSort"

  it should "work for doMain" in {
    // Expected time: 182 mSec
    val N = 128 * 1024 // DP Quicksort performs this sort in ...?
    val result: Seq[Int] = MergeSort.doMain(N)
    result.headOption shouldBe Some(1)
    result.lastOption shouldBe Some(N)
  }

  behavior of "MergeSortEager"

  it should "work very slowly for doMain" in {
    val N = 64 * 1024 // DP Quicksort performs this sort in 17.7 milliseconds (normalized time is: 16.9 nanoseconds)
    // Expected time: 5363 mSec (normalized: 5114 nanos)
    val result: Seq[Int] = MergeSortEager.doMain(N)
    result.headOption shouldBe Some(1)
    result.lastOption shouldBe Some(N)
  }
}