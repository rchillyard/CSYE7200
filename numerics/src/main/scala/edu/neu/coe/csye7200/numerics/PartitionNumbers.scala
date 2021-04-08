package edu.neu.coe.csye7200.numerics

/**
 * This object relates to the Euler Partitioning Numbers.
 * See https://en.wikipedia.org/wiki/Partition_function_(number_theory)
 *
 * See also https://youtu.be/iJ8pnCO0nTY
 */
object PartitionNumbers extends App {

  implicit class LazyListOps[X](xs: LazyList[X]) {
    def alternatingWith(ys: LazyList[X]): LazyList[X] = (xs zip ys) flatMap { case (a, b) => LazyList(a, b) }
  }

  val sequenceNumbers = LazyList.from(1, 2) alternatingWith (LazyList from 1)
  val partitionNumbers = sequenceNumbers.scanLeft(0)(_+_)

  val partitionNumbersWithIndex = partitionNumbers.zipWithIndex.take(667).toList
  partitionNumbersWithIndex.foreach{ case (a,b) => println(s"$b $a")}
}
