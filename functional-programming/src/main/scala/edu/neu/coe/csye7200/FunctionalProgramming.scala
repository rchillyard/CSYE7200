package edu.neu.coe.csye7200

import scala.util._


object FunctionalProgramming extends App {

  def evaluate_3_tenths = 1.0 / 10 + 2.0 / 10

  def multiply_by_10_over_3(x: Double) = x / 3 * 10

  val x = evaluate_3_tenths
  val y = multiply_by_10_over_3(x)
  println(s"$y != 1")

  //  val t = Try[String](throw new RuntimeException("my"))

  val t = Try("hello")
  val u = t.recoverWith {
    case e => Success(s"ignoring exception ${e.getLocalizedMessage}")
  }

  u.foreach(println(_))
}
