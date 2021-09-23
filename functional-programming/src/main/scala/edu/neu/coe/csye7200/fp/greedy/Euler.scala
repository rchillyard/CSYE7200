package edu.neu.coe.csye7200.fp.greedy

import scala.LazyList._
import scala.language.postfixOps
import scala.util.Random

object Euler extends App {

  def e(n: Int): Double = (from(1) map (1.0 / _)).scanLeft(1.0)(_ * _) take n sum

  println(e(20))
}


object RandomStrings extends App {
  val r = Random

  def randomString(r: Random): String = {
    (for (i <- 0 until 6) yield r.nextPrintableChar()).foldLeft("")(_ + _)
  }

  val strings = for (i <- 0 until 5) yield randomString(r)
  println(strings)
}
