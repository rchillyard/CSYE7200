package edu.neu.coe.csye7200.assthw

import edu.neu.coe.csye7200.assthw.Tries.zipTry
import scala.annotation.tailrec
import scala.collection.mutable.Queue
import scala.util.{Failure, Success, Try}

@main def runMergeSort(): Unit = println(MergeSort.doMain(100000))

/**
 * This is the most elegant version of MergeSort.
 * There are no explicit reversal operations.
 * Instead, the <code>result</code> parameter of the merge method is a Queue.
 *
 * NOTE however, that this implementation is not quite as fast as the Lazy version.
 * NOTE that this implementation (using a mutable Queue) is faster than using an immutable Queue, albeit not referentially transparent.
 *
 * @tparam X underlying type that must support Ordering via an implicit value.
 */
class MergeSort[X: Ordering]:

  /**
   * Sorts a given list of elements using the MergeSort algorithm. The algorithm divides
   * the input list into smaller sublists, sorts them recursively, and then merges the
   * sorted sublists back together to form a complete sorted list.
   *
   * @param xs the list of elements to be sorted, where the element type X is required to have an implicit Ordering.
   * @return a new list containing all elements of the input list xs in ascending order as defined by the implicit Ordering.
   */
  def sort(xs: List[X]): Try[List[X]] = xs match {
    case Nil | _ :: Nil =>
      Success(xs)
    case _ =>
      @tailrec
      def merge(result: Queue[X], l: List[X], r: List[X]): Try[List[X]] =
        (l, r) match {
          case (Nil, _) | (_, Nil) =>
            OneList(l, r) match {
              case OneList(x) =>
                Success(result ++: x)
              case _ =>
                Failure(new Exception("MergeSort: OneList result unexpected: " + result + " l: " + l + " r: " + r + ""))
            }
          case _ =>
            CompareLists(l, r) match {
              case CompareLists(h, t, other) =>
                merge(result += h, t, other)
              case _ =>
                Failure(new Exception("MergeSort: CompareLists result unexpected: " + result + " l: " + l + " r: " + r + ""))
            }
        }

      for {
        (l, r) <- Try(xs.splitAt(xs.length / 2))
        (ls, rs) <- zipTry(sort(l), sort(r))
        result <- merge(Queue.empty, ls, rs)
      } yield result
  }

/**
 * Companion object that demonstrates the MergeSort algorithm by ordering a list
 * of integers in ascending order. The `doMain` method generates a list
 * of integers, sorts them using the MergeSort algorithm, and outputs the result.
 */
object MergeSort {

  /**
   * Executes a sorting operation on a list of integers from 1 to `n` in descending order,
   * utilizing the MergeSort algorithm to return the list sorted in ascending order.
   *
   * @param n the number of integers, starting from 1 up to `n`, to be included in the list that will be sorted.
   * @return a sequence of integers sorted in ascending order.
   * @throws Any exception encountered during the sorting operation.
   */
  def doMain(n: Int): Seq[Int] = {
    val sorter = new MergeSort[Int]
    val list = (1 to n).toList.reverse
    sorter.sort(list) match {
      case Success(result) => result
      case Failure(e) => throw e
    }
  }
}


/**
 * A case class for comparing two lists of type `X` using an implicit ordering.
 *
 * @param left     the first list of elements to be compared
 * @param right    the second list of elements to be compared
 * @param ordering an implicit `Ordering` instance for elements of type `X`
 *                 that defines how the elements in the lists are compared
 *
 *                 This class is primarily used to compare elements of two lists and provides
 *                 structural equality for easy manipulation and comparison of lists.
 */
case class CompareLists[X](left: List[X], right: List[X])(implicit val ordering: Ordering[X])

/**
 * Companion object for the `CompareLists` case class. Provides a custom extractor pattern
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
  def unapply[X](cl: CompareLists[X]): Option[(X, List[X], List[X])] = (cl.left, cl.right) match {
    case (l, r) =>
      if (cl.ordering.compare(l.head, r.head) <= 0)
        Some((l.head, l.tail, r))
      else
        Some((r.head, r.tail, l))
  }
}

/**
 * Represents a pair of lists, only one of which is non-empty.
 *
 * @param left  a list of elements of type `X`
 * @param right a list of elements of type `X`
 * @tparam X the type of elements contained in both lists
 */
case class OneList[X](left: List[X], right: List[X])

/**
 * Companion object for the `OneList` case class.
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
  def unapply[X](cl: OneList[X]): Option[List[X]] = (cl.left,cl.right) match {
    case (_, Nil) => Some(cl.left)
    case (Nil, _) => Some(cl.right)
    case _ => None
  }
}