package edu.neu.coe.csye7200.assthw

import scala.annotation.tailrec
import scala.collection.mutable.Queue

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
class MergeSort[X: Ordering] {

    def sort(xs: List[X]): List[X] = xs match {
        case Nil | _ :: Nil => xs
        case _ =>
            @tailrec
            def merge(result: Queue[X], l: List[X], r: List[X]): List[X] =
                (l, r) match {
                    case (Nil, _) | (_, Nil) =>
                        OneList(l, r) match {
                            case OneList(x) => result ++: x
                            case _ => throw new Exception("MergeSort: OneList result unexpected: " + result + " l: " + l + " r: " + r + "")
                        }
                    case _ =>
                        CompareLists(l, r) match {
                            case CompareLists(h, t, other) => merge(result += h, t, other)
                            case _ => throw new Exception("MergeSort: CompareLists result unexpected: " + result + " l: " + l + " r: " + r + "")
                        }
                }

            val (l, r) = xs.splitAt(xs.length / 2)
            val (ls, rs) = (sort(l), sort(r))
            merge(Queue.empty, ls, rs)
    }
}

object MergeSort extends App {

    def doMain(n: Int): Seq[Int] = {
        val sorter = new MergeSort[Int]
        val list = (1 to n).toList.reverse
        sorter.sort(list)
    }

//    println(doMain(100000))
}

case class CompareLists[X](left: List[X], right: List[X])(implicit val ordering: Ordering[X])

object CompareLists {

    def unapply[X](cl: CompareLists[X]): Option[(X, List[X], List[X])] = (cl.left, cl.right) match {
        case (l, r) =>
            if (cl.ordering.compare(l.head, r.head) <= 0)
                Some((l.head, l.tail, r))
            else
                Some((r.head, r.tail, l))
    }
}

case class OneList[X](left: List[X], right: List[X])

object OneList {
    def unapply[X](cl: OneList[X]): Option[List[X]] = (cl.left,cl.right) match {
        case (_, Nil) => Some(cl.left)
        case (Nil, _) => Some(cl.right)
        case _ => None
    }
}