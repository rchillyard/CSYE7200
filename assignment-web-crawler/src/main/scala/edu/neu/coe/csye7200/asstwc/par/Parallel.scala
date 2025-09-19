package edu.neu.coe.csye7200.asstwc.par

import edu.neu.coe.csye7200.asstwc.par.Parallel.benchmark
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable
import scala.language.postfixOps

@main def runBenchmark(): Unit = {
  val m = 10
  val n = 10000000
  val expected: BigInt = (BigInt(2) * n * n * n + 3L * n * n + n) / 6
  println(s"Benchmark of sum of squares: N = $n with $m repetitions")

  val xs: List[Int] = LazyList from 1 take n toList
  val ys: List[BigInt] = xs map (x => BigInt(x) * x)
  val zs: immutable.ParSeq[BigInt] = ys.par

  val timeN = benchmark("Non-parallel", expected, m, ys.sum)
  val timeP = benchmark("Parallel", expected, m, zs.sum)

  println(s"Speed up with parallelization is by a factor of ${((timeN / timeP - 1) * 100).toInt}%")
}

/**
 * Object providing parallel computation-based utilities.
 */
object Parallel {
  /**
   * Runs a benchmarking operation for a given computation and compares the result against an expected value.
   * It evaluates the operation a specified number of times and calculates the average execution time.
   * If the computed result matches the expected value, the average execution time is printed.
   * Otherwise, an error message is printed with the actual and expected results.
   *
   * @param message  a descriptive message for the benchmark being performed.
   * @param expected the expected result of the computation.
   * @param m        the number of repetitions to perform the computation.
   * @param z        the computation to be benchmarked. This is a by-name parameter to allow deferred execution.
   * @return the average execution time of the computation in milliseconds.
   */
  def benchmark(message: String, expected: BigInt, m: Int, z: => BigInt): Double = {
    val (sum, time) = m times z
    if (sum == expected)
      println(s"$message: average time: $time mSecs")
    else {
      println(s"$message: error: $sum, expected: $expected")
    }
    time
  }
}
