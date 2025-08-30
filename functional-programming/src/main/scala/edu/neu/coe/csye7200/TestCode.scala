package edu.neu.coe.csye7200

object TestCode extends App {

  import scala.language.implicitConversions
  // For some reason it is best not to give a type here (maybe because you can't chain implicit conversions)
  implicit def stringToInt(x: String) = x.toInt

  val fortyTwo: Int = "42"
  println(fortyTwo + 6)
}