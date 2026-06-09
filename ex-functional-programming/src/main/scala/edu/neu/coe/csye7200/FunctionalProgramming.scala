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

sealed trait Wrapper[T] {

  def unit[U](u: U): Wrapper[U]

  def flatMap[U](f: T => Wrapper[U]): Wrapper[U]

  def map[U](f: T => U): Wrapper[U] = flatMap(t => unit(f(t)))
}

case class MyWrapper[T](x: T) extends Wrapper[T] {
  def unit[U](u: U): Wrapper[U] = MyWrapper[U](u)

  def flatMap[U](f: T => Wrapper[U]): Wrapper[U] = f(x)
}

object MyWrapper extends App {
  val myWrapper1: MyWrapper[Int] = MyWrapper[Int](0)
  val myWrapper2: Wrapper[String] = myWrapper1.map(_.toString)
  println(myWrapper2)
}