package edu.neu.coe.csye7200.asstwc.par

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable
import scala.language.postfixOps

object Parallel extends App {

  val m = 10
  val n = 10000000
  val expected: BigInt = (BigInt(2) * n * n * n + 3L * n * n + n) / 6
  println(s"Benchmark of sum of squares: N = $n with $m repetitions")

  val xs: List[Int] = LazyList from 1 take n toList
  val ys: List[BigInt] = xs map (x => BigInt(x) * x)
  val zs: immutable.ParSeq[BigInt] = ys.par

  val timeN = benchmark("Non-parallel", m, ys.sum)
  val timeP = benchmark("Parallel", m, zs.sum)

  println(s"Speed up with parallelization is by a factor of ${((timeN / timeP - 1) * 100).toInt}%")

  def benchmark(message: String, m: Int, z: => BigInt) = {
    val (sum, time) = m times z
    if (sum == expected)
      println(s"$message: average time: $time mSecs")
    else {
      println(s"$message: error: $sum, expected: $expected")
    }
    time
  }
}
