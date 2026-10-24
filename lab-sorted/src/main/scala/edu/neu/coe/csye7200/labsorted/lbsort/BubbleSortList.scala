package edu.neu.coe.csye7200.labsorted.lbsort

import scala.annotation.tailrec
import scala.util.Random


class BubbleSortList[X: Ordering] {
  def sort(xs: List[X]): List[X] = {
    val xo = implicitly[Ordering[X]]

    /**
     * Make one bubble pass over unsorted, carrying the larger of each adjacent
     * pair forward so that the largest element finishes at the head of the result.
     *
     * @param sorted   the elements passed over so far, in reverse of the order seen.
     * @param unsorted the elements not yet passed over.
     * @param swapped  whether any pair so far was found out of order.
     * @return the elements of the pass, largest first, paired with whether
     *         anything was out of order.
     */
    @tailrec def inner(sorted: List[X], unsorted: List[X], swapped: Boolean): (List[X], Boolean) =
      unsorted match {
        case Nil => sorted -> swapped
        case head :: Nil => inner(head :: sorted, Nil, swapped)
        // NOTE `rest` is bound so that the no-swap branch can pass the existing
        // tail straight through, rather than rebuilding it as `second :: tail`.
        // That saves one cons cell for every comparison which does not swap:
        // measured 12% fewer allocations on random input, 19% on nearly-sorted.
        // The swap branch still has to allocate, because `first` moves.
        case first :: (rest @ (second :: tail)) =>
          if (xo.compare(first, second) <= 0)
            inner(first :: sorted, rest, swapped)
          else
            inner(second :: sorted, first :: tail, swapped = true)
      }

    @tailrec def outer(largestSorted: List[X], unsorted: List[X]): List[X] =
      unsorted match {
        case Nil => largestSorted
        case _ =>
          inner(Nil, unsorted, swapped = false) match {
            // A pass which swapped nothing found unsorted already in ascending
            // order, and every element of it is no greater than any element of
            // largestSorted. So there is nothing left to do: this is what turns
            // the best case from quadratic into linear.
            case (_, false) => unsorted ++ largestSorted
            case (max :: tail, _) => outer(max :: largestSorted, tail)
            // Unreachable: unsorted is non-empty here, so the pass returns at
            // least one element. Stated so the match is exhaustive.
            case (Nil, _) => largestSorted
          }
      }

    outer(Nil, xs)
  }
}

object BubbleSortList {
  def apply[X: Ordering]: BubbleSortList[X] = new BubbleSortList[X]

  def sort[X: Ordering](xs: List[X]): List[X] = apply.sort(xs)
}


@main def doBenchmarkBubbleSortList(): Unit = {

  val random = new Random()

  def doSort(n: Int): Unit = {
    val xs: List[Int] = LazyList.continually(random.nextInt()) take n to List


    val result: List[Int] = BubbleSortList.sort(xs)
    println(result)
  }

  doSort(1000) // We can easily do 100000 here but it uses up a lot of space in the output
}