package edu.neu.coe.csye7200.assthw

import edu.neu.coe.csye7200.assthw.Tries.zipTry
import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer}
import scala.util.{Failure, Success, Try}

type Merger[X] = (List[X], List[X]) => Ordering[X] => List[X]

@main
def runMergeSortMutable(): Unit =
  val name = "Mutable MergeSort"
  println(if MergeSort.doMain(1_000_000)(MergeSort.mergeMutable) then name + " OK" else name + "Not OK") : Unit

@main
def runMergeSortImmutable(): Unit =
  val name = "Immutable MergeSort"
  println(if MergeSort.doMain(1_000_000)(MergeSort.mergeImmutable) then name + " OK" else name + "Not OK"): Unit

@main
def runMergeSortUnsafe(): Unit =
  val name = "Unsafe MergeSort"
  println(if MergeSort.doMain(1_000)(MergeSort.mergeSimple) then name + " OK" else name + "Not OK"): Unit

/**
 * Performs a merge sort, delegating the merge step to a pluggable `merger` strategy.
 *
 * See [[MergeSort.mergeSimple]] (not tail-recursive; only safe for short lists -- see its
 * own doc), [[MergeSort.mergeImmutable]] (tail-recursive, `List`-based, currently the
 * fastest of the three), and [[MergeSort.mergeMutable]] (tail-recursive, backed by a
 * mutable `ArrayBuffer`).
 *
 * NOTE: `mergeMutable` was assumed to be the fastest merger -- avoiding `mergeImmutable`'s
 * explicit final reversal -- but measured benchmarks (1M elements) show it's actually
 * slower than `mergeImmutable`, even after switching its accumulator from `Queue` to
 * `ArrayBuffer`. The per-element append plus the final buffer-to-list conversion appear to
 * cost more than `List`'s cons-then-reverse, which allocates nothing but cons cells and
 * never touches an array. Moral: measure before trusting an assumption about mutable vs.
 * immutable performance.
 *
 * @param merger the Merger instance used to merge sorted sublists.
 * @tparam X underlying type that must support Ordering via an implicit value.
 */
class MergeSort[X](merger: Merger[X])(using Ordering[X]):

  def sort(l: List[X]): Try[List[X]] =
    mergesort(l)

  /**
   * Sorts a given list of elements using the MergeSort algorithm. The algorithm divides
   * the input list into smaller sublists, sorts them recursively, and then merges the
   * sorted sublists back together to form a complete sorted list.
   *
   * @param xs the list of elements to be sorted, where the element type X is required to have an implicit Ordering.
   * @return a new list containing all elements of the input list xs in ascending order as defined by the implicit Ordering.
   */
  def mergesort(xs: List[X]): Try[List[X]] = xs match {
    case Nil | _ :: Nil =>
      Success(xs)
    case _ =>
      for
        (l, r) <- Try(xs.splitAt(xs.length / 2))
        (ls, rs) <- zipTry(mergesort(l), mergesort(r))
        sorted <- Try(merger(ls, rs)(summon[Ordering[X]]))
      yield sorted
  }

/**
 * Companion object that demonstrates the MergeSort algorithm by ordering a list
 * of integers in ascending order. The `doMain` method generates a list
 * of integers, sorts them using the MergeSort algorithm, and outputs the result.
 */
object MergeSort {

  /**
   * Merges two sorted lists into a single sorted list.
   * This function assumes that both input lists are already sorted according to their natural ordering
   * or a custom ordering defined by the implicit `Ordering[X]` in scope.
   * The result is a new sorted list containing all elements from both input lists.
   *
   * NOTE: This function is not tail-recursive and may cause stack overflow for very large lists.
   * NOTE: a `StackOverflowError` cannot be handled by the `Try` mechanism because it is a fatal error.
   *
   * @param l the first sorted list to merge
   * @param r the second sorted list to merge
   * @return a new sorted list containing all elements from `l` and `r` in sorted order
   */
  def mergeSimple[X: Ordering](l: List[X], r: List[X]): List[X] =
    (l, r) match {
      case (Nil, _) => r
      case (_, Nil) => l
      case (hl :: tl, hr :: tr) =>
        if Ordering[X].lt(hl, hr)
        then hl :: mergeSimple(tl, r) // NOTE: not tail recursive
        else hr :: mergeSimple(l, tr) // NOTE: not tail recursive
    }

  /**
   * Merges two sorted lists into a single sorted list.
   * This function assumes that both input lists are already sorted according to
   * their natural ordering or a custom ordering defined by the implicit `Ordering[X]` in scope.
   * The function is implemented in a tail-recursive manner.
   *
   * @param l the first sorted list to merge
   * @param r the second sorted list to merge
   * @return a new sorted list containing all elements from `l` and `r` in sorted order
   */
  def mergeImmutable[X: Ordering](l: List[X], r: List[X]): List[X] =
    @tailrec
    def inner(result: List[X], l: List[X], r: List[X]): List[X] =
      (l, r) match
        case (Nil, Nil) => result.reverse
        case (Nil, _) => result.reverse ++ r
        case (_, Nil) => result.reverse ++ l
        case (h1 :: t1, h2 :: t2) =>
          if Ordering[X].compare(h1, h2) <= 0
          then inner(h1 :: result, t1, r)
          else inner(h2 :: result, l, t2)
    inner(List.empty,l, r)

  /**
   * Merges two sorted lists into a single sorted list in a mutable and tail-recursive manner.
   * This function assumes that both input lists are already sorted according to their natural ordering
   * or a custom ordering defined by the implicit `Ordering[X]` in scope.
   *
   * @param l the first sorted list to merge
   * @param r the second sorted list to merge
   * @return a new sorted list containing all elements from `l` and `r` in sorted order
   */
  def mergeMutable[X: Ordering](l: List[X], r: List[X]): List[X] =
    @tailrec
    def inner(result: ArrayBuffer[X], l: List[X], r: List[X]): List[X] =
      (l, r) match {
        case OneList(x) =>
          result ++: x
        case CompareLists(h, t, other) =>
          inner(result += h, t, other)
        case _ =>
          throw new Exception("MergeSort: CompareLists result unexpected: " + result + " l: " + l + " r: " + r + "")
      }
    inner(ArrayBuffer.empty[X], l, r)

  /**
   * Executes a sorting operation on a list of integers from 1 to `n` in descending order,
   * utilizing the MergeSort algorithm to return the list sorted in ascending order.
   *
   * @param size the number of integers, starting from 1 up to `size`, to be included in the list that will be sorted.
   * @param merger the function used to merge two sorted lists into a single sorted list.
   * @return true if all 10 timed runs produced a correctly-sorted result, false if any run's result failed the sortedness check
   *         (a run that throws, other than a fatal error such as StackOverflowError, is caught and also counts as false).
   */
  def doMain(size: Int)(merger: (List[Int], List[Int]) => Ordering[Int] => List[Int]): Boolean = {
    val sorter = new MergeSort[Int](merger)
    val list = (1 to size).toList.reverse
    println(s"MergeSort of $size elements")
    import Timer._
    given foldable: Foldable[Boolean] = new Foldable[Boolean] {
      def combine: (Boolean, Boolean) => Boolean =
        (x, y) => x && y
      def zero = true
    }
    10.times{
      // sort uses merge sort
      sorter.mergesort(list) match {
        case Success(result) =>
          assertSorted(result)
        case Failure(e) =>
          System.err.println(e.getLocalizedMessage)
          false
      }
    }
  }

  private def assertSorted(xs: Seq[Int]): Boolean =
    val zipped = xs zip xs.tail
    zipped.forall { case (a, b) => a <= b }
}

/**
 * Extractor object for two non-Nil lists. Provides a custom extractor pattern
 * for splitting and comparing elements from two lists based on their ordering.
 *
 * The `unapply` method attempts to extract the smallest head element between the two
 * lists while maintaining the sorted order relative to the implicit `Ordering` provided
 * in the `CompareLists` instance.
 *
 */
object CompareLists {

  /**
   * Extractor method for the `CompareLists` case class. Evaluates and extracts the smallest head element
   * from the two lists, rearranging the lists to maintain their relative order of comparison.
   *
   * @param cl the `CompareLists` instance containing two lists and the implicit ordering used for comparison
   * @return an `Option` containing a tuple of the smallest head element, the updated first list,
   *         and the updated second list, or `None` if the input does not match the pattern
   */
  def unapply[X: Ordering](t: (List[X], List[X])): Option[(X, List[X], List[X])] = t match {
    case (Nil, _) => None
    case (_, Nil) => None
    case (l, r) =>
      if (Ordering[X].compare(l.head, r.head) <= 0)
        Some((l.head, l.tail, r))
      else
        Some((r.head, r.tail, l))
  }
}

/**
 * Extractor for a single list.
 * Provides an `unapply` method for extracting the non-empty list from a `OneList` instance.
 */
object OneList {
  /**
   * Extractor method to match and retrieve the non-empty list from a `OneList` instance.
   *
   * @param cl an instance of `OneList` containing two lists (`left` and `right`), where at least one is empty
   * @return an `Option` containing the non-empty list from the `OneList` instance if exactly one list is empty,
   *         or `None` if both lists are non-empty
   */
  def unapply[X](t: (List[X], List[X])): Option[List[X]] = t match {
    case (xs, Nil) =>
      Some(xs)
    case (Nil, xs) =>
      Some(xs)
    case _ => 
      None
  }
}