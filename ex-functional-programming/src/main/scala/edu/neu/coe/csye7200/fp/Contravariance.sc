package edu.neu.coe.csye7200

import scala.util._

object Contravariance {
  println("Welcome to the Contravariance worksheet")

  def f1(x: Int)(s: String) = Try(x + s.toInt)

  val f1a: Int => String => Try[Int] = f1
  val r1 = f1a(3)("2")

  def f2(x: String)(s: String) = Try(x.toInt + s.toInt)

  val f2a: String => String => Try[Int] = f2
  val r2 = f2a("3")("2")
  val fs: Seq[String with Int => String => Try[Int]] = Seq(f1a, f2a)
  val xs: Seq[Any] = Seq(3, "3")
  for {(f, x) <- fs zip xs} yield f(x)("2")

}