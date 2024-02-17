package edu.neu.coe.csye7200.labsorted.lbsort

import scala.annotation.tailrec
import scala.util.Random


class BubbleSortList[X: Ordering] {
    def sort(xs: List[X]): List[X] = {
        @tailrec def inner(sorted: List[X], unsorted: List[X]): List[X] =
          unsorted match {
            case Nil => sorted
            case head :: Nil => inner(head :: sorted, Nil)
            case first :: second :: tail =>
              if (implicitly[Ordering[X]].compare(first, second) <= 0)
                inner(first :: sorted, second :: tail)
              else
                inner(second :: sorted, first :: tail)
          }
        @tailrec def outer(largestSorted: List[X], unsorted: List[X]): List[X] =
          unsorted match {
            case Nil => largestSorted
            case _ =>
              val max :: tail = inner(Nil, unsorted)
              outer(max :: largestSorted, tail)
          }
        outer(Nil, xs)
      }
}

object BubbleSortList {
  def apply[X: Ordering]: BubbleSortList[X] = new BubbleSortList[X]

  def sort[X: Ordering](xs: List[X]): List[X] = apply.sort(xs)
}


object BenchmarkBubbleSortList extends App {

  val random = new Random()

  def doSort(n: Int) {
    val xs: List[Int] = LazyList.continually(random.nextInt()) take n to List


    val result: List[Int] = BubbleSortList.sort(xs)
//    println(result)
  }

  doSort(100000)
}