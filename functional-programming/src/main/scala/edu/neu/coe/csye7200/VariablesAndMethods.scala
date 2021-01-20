package edu.neu.coe.csye7200

object VariablesAndMethods extends App {

  val xs = List(1, 2, 3, 4)

  val mean = xs.sum.toDouble / xs.length // memoization by "variable"

  def sqr(x: Double) = x * x  // expression evaluation by "method"

  val variance = (for (x <- xs) yield sqr(x - mean)).sum / xs.length

  val stdDev = math.sqrt(variance)

  println(s"mean: $mean, stdDev: $stdDev")

}
