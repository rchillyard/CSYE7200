package edu.neu.coe.csye7200.numerics

import scala.language.postfixOps

/**
  * Object which is able to evaluate pi/4 by adding the following terms:
  *   Sum over k = 1 to N (atan (1 / F2k+1))
  *   where Fi is the ith Fibonacci number.
  *
  * See http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/fibpi.html#section2
  */
object FibonacciPi {
  val fibonacci: LazyList[BigInt] = 0L #:: fibonacci.scanLeft(BigInt(1L))(_ + _)

  /**
    * Evaluate pi/4 by taking the first n terms from the series described above.
    *
    * @param n the number of terms to add together. There is no theoretical limit to the value of n.
    * @return a number close to pi/4.
    */
  def piBy4(n: Int): BigDecimal = {
    val fibOdd: Iterator[BigInt] = fibonacci drop 1 sliding(1, 2) map (_.head)
    val terms: Iterator[BigDecimal] = fibOdd drop 1 map (f => math.atan(1.0 / f.toDouble))
    terms take n sum
  }
}

object PiFib extends App {
  println(s"pi by Fibonacci: ${FibonacciPi.piBy4(20000) * 4}")
  println(s"pi to 30 places: 3.141592653589793238462643383280")
}
