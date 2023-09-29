package edu.neu.coe.csye7200.labsorted.benchmark

import scala.annotation.tailrec

object Factorial extends App {

  def factorial(n: Int): BigInt = {
    @tailrec
    def inner(r: BigInt, i: Int): BigInt = i match {
      case 0 => r
      case _ => inner(r * i, i - 1)
    }
    inner(1, n)
  }

  println(factorial(10))

}