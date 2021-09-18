package edu.neu.coe.csye7200

import scala.language.implicitConversions

/**
  * @author scalaprof
  */
object Implicits extends App {
  val x = "1" + 2
  println(x)

  def myAdd(x: Int, y: Int): Int = x + y

  // For some reason it is best not to give a type here
  implicit def stringToInt(x: String) = x.toInt

  println(myAdd("1", "2"))

  val u = U(10)

  import T._

  println(u.double)

  implicit class V(v: Int) {
    override def toString: String = s"V: $v"
  }

  val v: V = 10
  println(v)
}

case class T(t: Int) {
  def double: Int = 2 * t
}

import scala.language.implicitConversions

object T {
  implicit def conv(u: U): T = T(u.u)
}

case class U(u: Int)
