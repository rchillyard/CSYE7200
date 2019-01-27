package edu.neu.coe.csye7200

/**
  * Created by scalaprof on 10/10/16.
  */
object Invert extends App {

  val iss1 = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
  val iss2: List[List[Int]] = for (is <- iss1) yield for (i <- is) yield i
  println(iss1, iss2)
}
