package edu.neu.coe.csye7200

/**
  * Created by scalaprof on 1/6/17.
  */

trait Fuzzy[X] extends Shape {
  def estimate: X

  def fuzz: X
}

trait Value[X] {
  def toDouble: Double

  def fromDouble(x: Double): X
}

trait Shape {
  def pdf(x: Double): Double
}

//case class FuzzyValue[X : Value, Y : Shape](estimate: X, fuzz: X) extends Fuzzy[X] {
//  def pdf(x: Double): Double = implicitly[Shape].pdf(p)
//}
//
