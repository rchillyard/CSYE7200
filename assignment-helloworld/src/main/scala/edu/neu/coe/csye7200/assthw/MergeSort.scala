package edu.neu.coe.csye7200.assthw

class MergeSort[X: Ordering] {

    def sort(xs: List[X]): List[X] = xs match {
        case Nil => xs
        case _ :: Nil => xs
        case _ =>
            def merge(result: List[X], l: List[X], r: List[X]): List[X] =
                (l, r) match {
                    case (Nil, Nil) => result.reverse
                    case (Nil, _) => result.reverse ++ r
                    case (_, Nil) => result.reverse ++ l
                    case (h1 :: t1, h2 :: t2) =>
                        if (implicitly[Ordering[X]].compare(h1, h2) <= 0)
                            merge(h1 :: result, t1, r)
                        else
                            merge(h2 :: result, l, t2)
                }

            val (l, r) = xs.splitAt(xs.length / 2)
            val (ls, rs) = (sort(l), sort(r))
            merge(Nil, ls, rs)
    }
}

object MergeSort extends App {
    val sorter = new MergeSort[Int]
    val list = (1 to 100000).toList.reverse
    val sorted = sorter.sort(list)
    println(sorted)
}