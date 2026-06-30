package edu.neu.coe.csye7200.assthw

import scala.annotation.tailrec
import scala.collection.immutable.Seq

class MergeSortEager[X: Ordering] {

    def sort(xs: List[X]): List[X] = xs match {
        case Nil | _ :: Nil => xs
        case _ =>
            @tailrec
            def merge(result: List[X], l: List[X], r: List[X]): List[X] =
                (l, r) match {
                    case (_, Nil) => result ++ l
                    case (Nil, _) => result ++ r
                    case (h1 :: t1, h2 :: t2) =>
                        if (Ordering[X].compare(h1, h2) <= 0)
                            merge(result :+ h1, t1, r) // NOTE slow but result will be in proper order.
                        else
                            merge(result :+ h2, l, t2) // NOTE slow but result will be in proper order.
                }

            val (l, r) = xs.splitAt(xs.length / 2)
            val (ls, rs) = (sort(l), sort(r))
            merge(Nil, ls, rs)
    }
}

@main def doMergeSortEager(): Unit = {

    def doMain(n: Int): Seq[Int] = {
        val sorter = new MergeSortEager[Int]
        val list = (1 to n).toList.reverse
        sorter.sort(list)
    }

    println(doMain(100)) // TODO 10000 to make it more interesting
}
