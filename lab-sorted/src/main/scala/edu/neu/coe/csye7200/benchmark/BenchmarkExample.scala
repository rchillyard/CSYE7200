/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.benchmark

import scala.language.implicitConversions

/**
  * Declaration of implicit class Rep within Benchmark.
  *
  * Created by scalaprof on 8/17/16.
  */
object BenchmarkExample extends App {
  println(s"ave time for 40! is ${10000.times(Factorial.factorial(40))} nanosecs")
}

