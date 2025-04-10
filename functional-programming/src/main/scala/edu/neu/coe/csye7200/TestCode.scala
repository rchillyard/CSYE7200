package edu.neu.coe.csye7200

object TestCode extends App {

  import scala.language.implicitConversions
  implicit def stringToInt(x: String) = x.toInt

  val fortyTwo: Int = "42"
  println(fortyTwo + 6)
}