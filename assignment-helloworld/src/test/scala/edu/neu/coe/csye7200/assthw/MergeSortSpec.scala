package edu.neu.coe.csye7200.assthw

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MergeSortSpec extends AnyFlatSpec with should.Matchers {

  val N = 64 * 1024 // DP Quicksort performs this sort in 17.7 milliseconds (normalized time is: 16.9 nanoseconds)

  behavior of "MergeSort"

  it should "sort empty list" in {
    new MergeSort[Int].sort(Nil) shouldBe Nil
  }

  it should "sort singleton list" in {
    val list = List(1)
    new MergeSort[Int].sort(list) shouldBe list
  }

  it should "sort doubleton list A" in {
    val list = List(1, 2)
    new MergeSort[Int].sort(list) shouldBe list
  }

  it should "sort doubleton list B" in {
    val expected = List(1, 2)
    val list = List(2, 1)
    new MergeSort[Int].sort(list) shouldBe expected
  }

  it should "sort tripleton list A" in {
    val list = List(1, 2, 3)
    new MergeSort[Int].sort(list) shouldBe list
  }

  it should "sort tripleton list B" in {
    val expected = List(1, 2, 3)
    val list = List(2, 1, 3)
    new MergeSort[Int].sort(list) shouldBe expected
  }

  it should "sort tripleton list C" in {
    val expected = List(1, 2, 3)
    val list = List(2, 3, 1)
    new MergeSort[Int].sort(list) shouldBe expected
  }

  it should "sort tripleton list D" in {
    val expected = List(1, 2, 3)
    val list = List(3, 2, 1)
    new MergeSort[Int].sort(list) shouldBe expected
  }

  it should "sort tripleton list E" in {
    val expected = List(1, 2, 3)
    val list = List(3, 1, 2)
    new MergeSort[Int].sort(list) shouldBe expected
  }

  it should "sort tripleton list F" in {
    val expected = List(1, 2, 3)
    val list = List(3, 2, 1)
    new MergeSort[Int].sort(list) shouldBe expected
  }

  it should "work for doMain" in {
    // Expected time: 82 mSec
    val result: Seq[Int] = MergeSort.doMain(N)
    result.headOption shouldBe Some(1)
    result.lastOption shouldBe Some(N)
  }

  behavior of "MergeSortStackOverflow"

  it should "sort empty list" in {
    new MergeSortStackOverflow[Int].sort(Nil) shouldBe Nil
  }

  it should "sort singleton list" in {
    val list = List(1)
    new MergeSortStackOverflow[Int].sort(list) shouldBe list
  }

  it should "sort doubleton list A" in {
    val list = List(1, 2)
    new MergeSortStackOverflow[Int].sort(list) shouldBe list
  }

  it should "sort doubleton list B" in {
    val expected = List(1, 2)
    val list = List(2, 1)
    new MergeSortStackOverflow[Int].sort(list) shouldBe expected
  }

  it should "sort tripleton list A" in {
    val list = List(1, 2, 3)
    new MergeSortStackOverflow[Int].sort(list) shouldBe list
  }

  it should "sort tripleton list B" in {
    val expected = List(1, 2, 3)
    val list = List(2, 1, 3)
    new MergeSortStackOverflow[Int].sort(list) shouldBe expected
  }

  it should "sort tripleton list C" in {
    val expected = List(1, 2, 3)
    val list = List(2, 3, 1)
    new MergeSortStackOverflow[Int].sort(list) shouldBe expected
  }

  it should "sort tripleton list D" in {
    val expected = List(1, 2, 3)
    val list = List(3, 2, 1)
    new MergeSortStackOverflow[Int].sort(list) shouldBe expected
  }

  it should "sort tripleton list E" in {
    val expected = List(1, 2, 3)
    val list = List(3, 1, 2)
    new MergeSortStackOverflow[Int].sort(list) shouldBe expected
  }

  it should "sort tripleton list F" in {
    val expected = List(1, 2, 3)
    val list = List(3, 2, 1)
    new MergeSortStackOverflow[Int].sort(list) shouldBe expected
  }

  it should "work for doMain" in {
    val N = 10000
    val result: Seq[Int] = MergeSortStackOverflow.doMain(N)
    result.headOption shouldBe Some(1)
    result.lastOption shouldBe Some(N)
  }

  it should "fail for doMain" in {
    a [StackOverflowError] should be thrownBy MergeSortStackOverflow.doMain(N)
  }

  behavior of "MergeSortEager"

  it should "sort empty list" in {
    new MergeSortEager[Int].sort(Nil) shouldBe Nil
  }

  it should "sort singleton list" in {
    val list = List(1)
    new MergeSortEager[Int].sort(list) shouldBe list
  }

  it should "sort doubleton list A" in {
    val list = List(1, 2)
    new MergeSortEager[Int].sort(list) shouldBe list
  }

  it should "sort doubleton list B" in {
    val expected = List(1, 2)
    val list = List(2, 1)
    new MergeSortEager[Int].sort(list) shouldBe expected
  }

  it should "sort tripleton list A" in {
    val list = List(1, 2, 3)
    new MergeSortEager[Int].sort(list) shouldBe list
  }

  it should "sort tripleton list B" in {
    val expected = List(1, 2, 3)
    val list = List(2, 1, 3)
    new MergeSortEager[Int].sort(list) shouldBe expected
  }

  it should "sort tripleton list C" in {
    val expected = List(1, 2, 3)
    val list = List(2, 3, 1)
    new MergeSortEager[Int].sort(list) shouldBe expected
  }

  it should "sort tripleton list D" in {
    val expected = List(1, 2, 3)
    val list = List(3, 2, 1)
    new MergeSortEager[Int].sort(list) shouldBe expected
  }

  it should "sort tripleton list E" in {
    val expected = List(1, 2, 3)
    val list = List(3, 1, 2)
    new MergeSortEager[Int].sort(list) shouldBe expected
  }

  it should "sort tripleton list F" in {
    val expected = List(1, 2, 3)
    val list = List(3, 2, 1)
    new MergeSortEager[Int].sort(list) shouldBe expected
  }
}