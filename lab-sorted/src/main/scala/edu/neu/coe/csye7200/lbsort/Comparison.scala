package edu.neu.coe.csye7200.lbsort

import scala.language.{implicitConversions, postfixOps}

trait Comparison extends (() => Option[Boolean]) {

  /**
    * Method to yield logical AND.
    *
    * @param c the other Comparison (eagerly evaluated)
    * @return a Comparison according to Kleenean logic
    */
  def &(c: Comparison): Comparison = Comparison(math.min(toInt, c.toInt))

  /**
    * Method to yield logical OR.
    *
    * @param c the other Comparison (eagerly evaluated)
    * @return a Comparison according to Kleenean logic
    */
  def |(c: Comparison): Comparison = Comparison(math.max(toInt, c.toInt))

  /**
    * Method to yield logical AND with short-circuit logic.
    *
    * @param c the other Comparison (lazily evaluated)
    * @return a Comparison according to Kleenean logic
    */
  def &&(c: => Comparison): Comparison

  /**
    * Method to yield logical OR with short-circuit logic.
    *
    * @param c the other Comparison (lazily evaluated)
    * @return a Comparison according to Kleenean logic
    */
  def ||(c: => Comparison): Comparison

  override def toString(): String = apply().toString

  /**
    * Method to return the Java-style value of this Comparison
    *
    * @return if Same then 0 else if Different(true) then -1 else 1
    */
  def toInt: Int

  /**
    * Method to compose this with another Comparison.
    * That is to say we yield either this or, in the case that this is Same, a default value of Comparison.
    *
    * @param c the other Comparison (lazily evaluated).
    * @return the composition of this and c.
    */
  def orElse(c: => Comparison): Comparison = Comparison(apply().orElse(c()))

  /**
    * Method to flip (i.e. negate) this Comparison.
    *
    * @return Same if this Comparison is Same else return Different(!less),
    */
  def flip: Comparison
}

case class Different(less: Boolean) extends Comparison {
  def apply(): Option[Boolean] = Some(less)

  def &&(c: => Comparison): Comparison = if (less) this else c

  def ||(c: => Comparison): Comparison = if (less) c else this

  def flip: Comparison = Different(!less)

  override def toInt: Int = if (less) -1 else 1
}

case object Same extends Comparison {
  def apply(): Option[Boolean] = None

  def &&(c: => Comparison): Comparison = c & this

  def ||(c: => Comparison): Comparison = c | this

  def flip: Comparison = this

  override def toInt: Int = 0
}

object Comparison {
  val more: Different = Different(false)
  val less: Different = Different(true)

  def apply(b: Boolean): Comparison = Different(b)

  def apply(x: Option[Boolean]): Comparison = x match {
    case Some(b) => apply(b);
    case _ => Same
  }

  def apply(x: Int): Comparison = x match {
    case 0 => Same;
    case _ => Comparison(Some(x < 0))
  }
}
