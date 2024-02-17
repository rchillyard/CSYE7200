package edu.neu.coe.csye7200.labsorted.lbsort

object BaselExercise {

  import LazyList._

  // Defining a lazy list to generate the series of 1/n^2

    val series: LazyList[Double] = LazyList.from(1).map(n => 1.0 / (n * n))
    // Calculating the cumulative sum of the series

    val cumulativeSum: LazyList[Double] = series.scanLeft(0.0)(_ + _)

    // Calculate the approximate value of Pi
    val piApproximation: LazyList[Double] = cumulativeSum.map(sum => Math.sqrt(6 * sum))

    // Printing the values of Pi at the 1000th Iteration
    def main(args: Array[String]): Unit = {
      var res = piApproximation.drop(1000 - 1).head
      println(res)
    }


}