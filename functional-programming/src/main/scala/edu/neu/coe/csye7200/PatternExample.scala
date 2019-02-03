/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200

object PatternExample extends App {
  decode(List(1, 2, -1))

  val m = Map(1 -> "uno", 2 -> "dos")
  for (kv <- m) decode(kv)

  def decode(xs: List[Int]): Unit =
    xs match {
      case Nil =>
      case h :: t => println(h); decode(t)
    }

  def decode(xs: (Int, String)): Unit =
    xs match {
      case (k, v) => println(s"$k: $v")
    }

}
