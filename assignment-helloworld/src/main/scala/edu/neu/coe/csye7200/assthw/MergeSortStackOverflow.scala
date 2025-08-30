package edu.neu.coe.csye7200.assthw

import scala.annotation.tailrec
import scala.collection.immutable.Seq

class MergeSortStackOverflow[X: Ordering] {

    def sort(xs: List[X]): List[X] = xs match {
        case Nil => xs
        case _ :: Nil => xs
        case _ =>
            def merge(l: List[X], r: List[X]): List[X] =
                (l, r) match {
                    case (_, Nil) => l
                    case (Nil, _) => r
                    case (h1 :: t1, h2 :: t2) =>
                        if (implicitly[Ordering[X]].compare(h1, h2) <= 0)
                            h1 :: merge(t1, r)
                        else
                            h2 :: merge(l, t2)
                }

            val (l, r) = xs.splitAt(xs.length / 2)
            val (ls, rs) = (sort(l), sort(r))
            merge(ls, rs)
    }
}

object MergeSortStackOverflow extends App {

    def doMain(n: Int): Seq[Int] = {
        val sorter = new MergeSortStackOverflow[Int]
        val list = (1 to n).toList.reverse
        sorter.sort(list)
    }

    println(doMain(100)) // TODO 10000 to make it more interesting
}