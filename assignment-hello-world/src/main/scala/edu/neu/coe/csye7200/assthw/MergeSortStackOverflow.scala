package edu.neu.coe.csye7200.assthw

import edu.neu.coe.csye7200.assthw.MergeSortStackOverflow.doMain
import scala.collection.immutable.Seq

/**
 * A class for performing merge sort on a `List` of elements of a generic type `X`.
 * The class requires an implicit `Ordering` for the type `X` to compare and sort elements.
 *
 * @tparam X the type of elements in the list, which must have an implicit `Ordering` available.
 */
class MergeSortStackOverflow[X: Ordering] {

  /**
   * Sorts a given list of elements of type `X` in ascending order using the merge sort algorithm.
   * This method requires an implicit `Ordering` for the type `X` to compare and sort elements.
   *
   * NOTE that this implementation is not tail-recursive.
   *
   * @param xs the list of elements of type `X` to be sorted
   * @return a new list of elements of type `X`, sorted in ascending order
   */
    def sort(xs: List[X]): List[X] = xs match {
        case Nil => xs
        case _ :: Nil => xs
        case _ =>
            def merge(l: List[X], r: List[X]): List[X] =
                (l, r) match {
                    case (_, Nil) => l
                    case (Nil, _) => r
                    case (h1 :: t1, h2 :: t2) =>
                        if (Ordering[X].compare(h1, h2) <= 0)
                            h1 :: merge(t1, r)
                        else
                            h2 :: merge(l, t2)
                }

            val (l, r) = xs.splitAt(xs.length / 2)
            val (ls, rs) = (sort(l), sort(r))
            merge(ls, rs)
    }
}

object MergeSortStackOverflow {

  def doMain(n: Int): Seq[Int] = {
    val sorter = new MergeSortStackOverflow[Int]
    val list = (1 to n).toList.reverse
    sorter.sort(list)
  }

}

@main def doMergeSortStackOverflow(): Unit = {

  println(doMain(1000)) // TODO 100000 to cause a StackOverflowError
}
