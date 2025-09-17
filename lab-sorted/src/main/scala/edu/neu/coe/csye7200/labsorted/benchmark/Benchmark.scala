package edu.neu.coe.csye7200.labsorted.benchmark

import scala.language.implicitConversions

/**
 * Declaration of Int extension within package object.
  *
  * Created by scalaprof on 8/17/16.
  */
object Benchmark extends App {
  println(s"ave time for 40! is ${10000.times(Factorial.factorial(40))} nanosecs")
}
