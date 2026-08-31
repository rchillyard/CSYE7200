package edu.neu.coe.csye7200.labsorted.lbsort

import scala.annotation.tailrec

/**
 * Insertion sort for immutable lists.
 *
 * NOTE the counterpart in Sorting.scala, `InsertionSort`, sorts an Array in
 * place. This one rebuilds Lists, so the costs are quite different, and it is
 * worth being explicit about them.
 *
 * Each element is inserted into the already-sorted prefix, and only that prefix
 * is rebuilt, as far as the insertion point. So the cost of one insertion is the
 * position found, not the length of the list. Averaged over a random input that
 * gives about n^2/4 comparisons and about n^2/4 cons cells.
 *
 * Compare [[BubbleSortList]], whose defining operation is the exchange of two
 * adjacent elements. An immutable list cannot exchange anything in place, so each
 * comparison rebuilds, giving about n^2/2 comparisons and about n^2 cons cells.
 * That is why this class, not that one, is the base case for [[MergeSortList]].
 *
 * This sort is stable.
 *
 * @tparam X the underlying type, which must have an Ordering.
 */
class InsertionSortList[X: Ordering] {

  private val xo = implicitly[Ordering[X]]

  /**
   * Sort xs. The input is not modified.
   *
   * Safe for a list of any length: see `insert`.
   *
   * @param xs the list to sort.
   * @return a new list containing the same elements in ascending order.
   */
  def sort(xs: List[X]): List[X] =
    xs.foldLeft(List.empty[X])((sorted, x) => insert(x, sorted))

  /**
   * Sort a list already known to be short.
   *
   * Identical in result to `sort`, but roughly half the allocation, because
   * `insertBounded` builds the rebuilt prefix once where `insert` builds it
   * twice. The price is that the insertion position goes on the stack, so the
   * caller must guarantee the list is short. [[MergeSortList]] can, because its
   * base case is bounded by the cutoff; a caller who cannot should use `sort`.
   *
   * Package-private for that reason: this is not a safe thing to offer generally.
   *
   * @param xs a short list.
   * @return a new list containing the same elements in ascending order.
   */
  private[lbsort] def sortBounded(xs: List[X]): List[X] =
    xs.foldLeft(List.empty[X])((sorted, x) => insertBounded(x, sorted))

  /**
   * Insert x into an already-sorted list, ahead of the first element that is
   * strictly greater. Going strictly greater rather than greater-or-equal is what
   * makes the sort stable: an element equal to x keeps its earlier position.
   *
   * NOTE tail-recursive, accumulating the skipped prefix in reverse and splicing
   * it back with reverse_:::. The obvious recursive form, `h :: insert(x, t)`, is
   * not tail-recursive and would put the insertion position on the stack -- fine
   * for the short lists MergeSortList hands over, but not for a long list sorted
   * by this class alone.
   *
   * @param x      the element to insert.
   * @param sorted a list already in ascending order.
   * @return sorted, with x inserted in place.
   */
  private def insert(x: X, sorted: List[X]): List[X] = {
    @tailrec def inner(before: List[X], after: List[X]): List[X] = after match {
      case Nil => before reverse_::: (x :: Nil)
      case h :: _ if xo.lt(x, h) => before reverse_::: (x :: after)
      case h :: t => inner(h :: before, t)
    }

    inner(Nil, sorted)
  }

  /**
   * Insert x into an already-sorted list, as `insert` does, but building the
   * rebuilt prefix once instead of twice.
   *
   * `h :: insertBounded(x, t)` cannot be tail-recursive -- the cons has to happen
   * after the recursive call returns -- so the insertion position is held on the
   * stack. In exchange, each skipped element costs one cons cell rather than the
   * two that accumulate-then-reverse costs. Only safe for short lists; see
   * `sortBounded`.
   *
   * @param x      the element to insert.
   * @param sorted a short list already in ascending order.
   * @return sorted, with x inserted in place.
   */
  private def insertBounded(x: X, sorted: List[X]): List[X] = sorted match {
    case Nil => x :: Nil
    case h :: _ if xo.lt(x, h) => x :: sorted
    case h :: t => h :: insertBounded(x, t)
  }
}

/**
 * Companion object to InsertionSortList.
 */
object InsertionSortList {
  def apply[X: Ordering]: InsertionSortList[X] = new InsertionSortList[X]

  def sort[X: Ordering](xs: List[X]): List[X] = apply.sort(xs)
}
