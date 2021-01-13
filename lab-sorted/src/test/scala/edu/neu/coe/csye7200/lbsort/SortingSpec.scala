/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.lbsort

import edu.neu.coe.csye7200.util.RandomState
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * @author scalaprof
  */
class SortingSpec extends AnyFlatSpec with Matchers with Futures with ScalaFutures {

  behavior of "Insertion Sort"

  it should "sort List[Int]" in {
    val list = Array(3, 1, 2)
    Sorting.insertionSort(list)
    list shouldBe Array(1, 2, 3)
  }
  it should "sort List[String]" in {
    val list = Array("b", "c", "a")
    Sorting.insertionSort(list)
    list shouldBe Array("a", "b", "c")
  }
  it should "sort List[Double] using create" in {
    val list = Array(3.0, 1.5, 2.4)
    Sorting.insertionSort(list)
    list shouldBe Array(1.5, 2.4, 3.0)
  }

  behavior of "Quick Sort"

  it should "sort List[Long]" in {
    val list = RandomState(0L).stream.take(100).toArray
    Sorting.quickSort(list)
    list.reverse.take(5) shouldBe Array(9054633673849498218L, 8937230293740383692L, 8613213585075034408L, 8543763135442756639L, 8358116205139703580L)
  }
}
