package edu.neu.coe.csye7200

object Closure extends App {

  // NOTE: this is suspicious
  // XXX I'm not sure what I intended here.
  // But as written it is a closure becuse it closes on y, albeit a forward reference.
  // Not a closure--this is false.
  val f0: Double => String = x => (x / y).toString

  // A closure
  val y = 2
  val f1: Double => String = x => (x / y).toString
  println(f1(Math.PI))

  // A closure but, since m is a method (not a variable), the value 1 isn't captured by f2.
  var z = 0

  def m = {
    z += 1; z
  }

  val f2: Double => String = x => (m * x).toString
  m
  println(f2(Math.PI))
}
