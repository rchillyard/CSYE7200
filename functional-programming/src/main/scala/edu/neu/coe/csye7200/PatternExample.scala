/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200

import scala.annotation.tailrec

object PatternExample extends App {
  decode(List(1, 2, -1))

  val m = Map(1 -> "uno", 2 -> "dos", 3 -> "tres")

  // Here, we have a pattern with a guard, although in a for-comprehension, the generator is embedded in the pattern.
  for (kv <- m if kv._1 %2 != 0) decode(kv)

  // Here we define first as 1 -> "uno" and rest as the remaining key-value pairs of m.
  val first :: rest = m.toSeq

  @tailrec
  def decode(xs: List[Int]): Unit =
    xs match {
      // A constant case pattern on the left hand side of =>
      case Nil =>
      // A simple case pattern, based on the case class ::, on the left hand side of =>
      case h :: t => println(h); decode(t)
    }

  def decode(xs: (Int, String)): Unit =
    xs match {
        // A simple case pattern but based on a Tuple2 on the left hand side of =>
      case (k, v) => println(s"$k: $v")
    }

  case class Complex(r: Double, i: Double)

  val pi = Complex(-1, 0)
  // Here we have a more complex pattern (excuse the pun) on the left-hand-side of a variable definition
  val Complex(minusOne, zero) = pi
  println(s"minusOne: $minusOne, zero: $zero")

}
