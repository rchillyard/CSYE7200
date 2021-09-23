package edu.neu.coe.csye7200.fp.factorial

/**
  * @author scalaprof
  */
class Factorial(val n: Int) extends (() => Long) {
  def apply(): Long = Factorial.factorial(n)
}

object Factorial extends App {
  println(new Factorial(5)())

  def factorial(n: Int) = {
    @scala.annotation.tailrec
    def inner(r: Long, n: Int): Long =
      if (n <= 1) r
      else inner(n * r, n - 1)

    inner(1L, n)
  }
}
