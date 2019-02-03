package edu.neu.coe.csye7200

import scala.annotation.tailrec

object Factorial extends App {
  println("Welcome to the Factorial worksheet")

  //> Welcome to the Factorial worksheet
  def factorial(x: Int): Long = {
    @tailrec def inner(r: Long, i: Int): Long =
      if (i == 0) 1 else inner(r * i, i - 1)

    inner(1L, x)
  }

  factorial(5)

  factorial(20)
}