package edu.neu.coe.csye7200

import scala.annotation.tailrec

/**
  * Created by scalaprof on 12/2/16.
  */
object Recursion extends App {

  def factorial(x: Int): Long = {
    @tailrec def inner(r: Long, i: Int): Long =
      if (i == 0) r else inner(r * i, i - 1)

    inner(1, x)
  }

  println(factorial(20)) //> res0: Long = 2432902008176640000
}
