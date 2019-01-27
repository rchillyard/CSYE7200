package edu.neu.coe.csye7200.oldrules

/**
  * @author robinhillyard
  */
sealed trait Operator[T] extends ((T, T) => Boolean)

case class LessThan() extends Operator[Double] {
  def apply(x: Double, y: Double): Boolean = x < y
}

case class GreaterThan() extends Operator[Double] {
  def apply(x: Double, y: Double): Boolean = x > y
}

case class Equals() extends Operator[Double] {
  def apply(x: Double, y: Double): Boolean = Math.abs(x - y) < 0.001
}

case class Matches() extends Operator[String] {
  def apply(x: String, y: String): Boolean = x == y
}

case class |[T](left: Operator[T], right: Operator[T]) {
  def apply(x: T, y: T): Boolean = if (left(x, y)) true else right(x, y)
}

case class &[T](left: Operator[T], right: Operator[T]) {
  def apply(x: T, y: T): Boolean = if (left(x, y)) right(x, y) else false
}

object Operator {
  def createNumeric(s: String): Operator[Double] = s match {
    case "=" => new Equals
    case "<" => new LessThan
    case ">" => new GreaterThan
    // FIXME figure out what's wrong here!
    case "<=" => |(LessThan.asInstanceOf[Operator[Double]], Equals.asInstanceOf[Operator[Double]]).asInstanceOf[Operator[Double]]
    case ">=" => |(GreaterThan.asInstanceOf[Operator[Double]], Equals.asInstanceOf[Operator[Double]]).asInstanceOf[Operator[Double]]
  }

  def createText(s: String): Operator[String] = s match {
    case "==" => new Matches
  }

}
