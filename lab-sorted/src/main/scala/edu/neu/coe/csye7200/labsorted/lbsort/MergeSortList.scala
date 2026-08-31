package edu.neu.coe.csye7200.labsorted.lbsort

import scala.annotation.tailrec

/**
 * Merge sort for immutable lists, deferring to [[InsertionSortList]] once a
 * sublist is short enough.
 *
 * Merge sort is the sort genuinely suited to lists: it needs only sequential
 * access, it is stable, and it is O(n log n). But it is not the fastest thing to
 * do with sixteen elements, because the splitting and merging have real constant
 * costs of their own. So below `cutoff` we hand over to a quadratic sort with
 * smaller constants -- the same tactic `MergeSort` in Sorting.scala applies to
 * Arrays with `startWidth = 8`, and `QuickSort` with `limit = 16`, and that
 * Timsort applies below 32.
 *
 * The base case is [[InsertionSortList]] rather than [[BubbleSortList]]
 * deliberately; see the note in InsertionSortList for the reason. Override
 * `sortShort` to substitute another.
 *
 * This sort is stable, provided the base case is.
 *
 * @param cutoff sublists of this length or shorter go to `sortShort`. A cutoff of
 *               1 or less disables the optimisation, giving plain merge sort.
 * @tparam X the underlying type, which must have an Ordering.
 */
class MergeSortList[X: Ordering](val cutoff: Int = MergeSortList.defaultCutoff) {

  private val xo = implicitly[Ordering[X]]
  private val shortSorter = new InsertionSortList[X]

  /**
   * Sort xs. The input is not modified.
   *
   * @param xs the list to sort.
   * @return a new list containing the same elements in ascending order.
   */
  def sort(xs: List[X]): List[X] = doSort(xs, xs.length)

  /**
   * Sort a sublist short enough to be handled by the base case.
   *
   * Protected so that a subclass can substitute a different base case, which is
   * what the cutoff benchmark does in order to compare candidates.
   *
   * @param xs a list of at most `cutoff` elements.
   * @return xs in ascending order.
   */
  protected def sortShort(xs: List[X]): List[X] = shortSorter.sortBounded(xs)

  /**
   * NOTE the length is threaded through the recursion rather than recomputed.
   * List.length is O(n), so calling it at every level would add an O(n log n)
   * term for no reason.
   */
  private def doSort(xs: List[X], n: Int): List[X] =
    if (n <= 1) xs
    else if (n <= cutoff) sortShort(xs)
    else {
      val half = n / 2
      val (left, right) = xs.splitAt(half)
      merge(doSort(left, half), doSort(right, n - half))
    }

  /**
   * Merge two ascending lists into one.
   *
   * Tail-recursive, accumulating in reverse and splicing with reverse_:::, so the
   * depth of a merge is not the length of its input. Takes from `as` when the
   * heads compare equal, which is what preserves stability.
   */
  private def merge(as: List[X], bs: List[X]): List[X] = {
    @tailrec def inner(acc: List[X], as: List[X], bs: List[X]): List[X] = (as, bs) match {
      case (Nil, _) => acc reverse_::: bs
      case (_, Nil) => acc reverse_::: as
      case (a :: ta, b :: _) if xo.lteq(a, b) => inner(a :: acc, ta, bs)
      case (_, b :: tb) => inner(b :: acc, as, tb)
    }

    inner(Nil, as, bs)
  }
}

/**
 * Companion object to MergeSortList.
 */
object MergeSortList {
  /**
   * The default cutoff, matching `QuickSort.limit` in Sorting.scala.
   */
  val defaultCutoff: Int = 16

  def apply[X: Ordering]: MergeSortList[X] = new MergeSortList[X]()

  /**
   * Construct a MergeSortList with an explicit cutoff.
   */
  def withCutoff[X: Ordering](cutoff: Int): MergeSortList[X] = new MergeSortList[X](cutoff)

  def sort[X: Ordering](xs: List[X]): List[X] = apply.sort(xs)
}
