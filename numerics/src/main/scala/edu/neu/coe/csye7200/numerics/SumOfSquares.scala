package edu.neu.coe.csye7200.numerics

import scala.language.postfixOps

/**
  * Created by scalaprof on 10/28/16.
  */
object SumOfSquares extends App {
  def sumOfSquares(xs: Seq[Double]) = xs map (x => x * x) sum

  println(sumOfSquares(Seq(1,2,3,4,5)))
}
