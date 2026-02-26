package edu.neu.coe.csye7200.labsorted.lbsort

import scala.language.{implicitConversions, postfixOps}

/**
 * Trait representing a Comparison, which can evaluate to an optional Boolean.
 * It supports Kleenean logic operations such as AND, OR, and negation, with both
 * eager and lazy evaluation mechanisms.
 */
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

/**
 * Represents a `Comparison` that is not equivalence, but depends on the value of the member `less`.
 * This class is a case class and a concrete implementation of the abstract `Comparison` trait.
 *
 * @param less a Boolean indicating whether the comparison condition is "less".
 */
case class Different(less: Boolean) extends Comparison {
  /**
   * Invokes the operation and determines the result of a specific condition.
   *
   * @return An Option containing a Boolean value. Returns Some(true) if `less` is  true,
   *         Some(false) if `less` is false.
   */
  def apply(): Option[Boolean] = Some(less)

  /**
   * Applies a logical AND operation with short-circuiting behavior.
   * If this Comparison is less, it returns this instance;
   * otherwise, it evaluates and returns the provided Comparison.
   *
   * @param c the other Comparison to evaluate (lazily evaluated)
   * @return a Comparison that represents the result of the logical AND operation
   */
  def &&(c: => Comparison): Comparison = if (less) this else c

  /**
   * Performs a logical OR operation with short-circuit evaluation on this `Comparison` instance.
   * If this `Comparison` evaluates to `less = false`, the provided `Comparison` `c` will be evaluated
   * and returned instead. Otherwise, this `Comparison` instance is returned.
   *
   * @param c the other `Comparison` instance to evaluate when this `Comparison` evaluates to `less = false` (lazily evaluated).
   * @return the result of the logical OR operation, determined according to Kleenean logic.
   */
  def ||(c: => Comparison): Comparison = if (less) c else this

  /**
   * Flips the current Comparison, negating the `less` property.
   *
   * @return a new Comparison instance where the logical state is inverted. For example,
   *         if the current Comparison is `Different(true)`, it will return `Different(false)`.
   */
  def flip: Comparison = Different(!less)

  /**
   * Converts the comparison result to an integer value.
   * If the comparison evaluates to "less", the result is -1.
   * Otherwise, the result is 1.
   *
   * @return an integer representation of the comparison result
   */
  def toInt: Int = if (less) -1 else 1
}

case object Same extends Comparison {
  def apply(): Option[Boolean] = None

  def &&(c: => Comparison): Comparison = c & this

  def ||(c: => Comparison): Comparison = c | this

  def flip: Comparison = this

  def toInt: Int = 0
}

object Comparison {
  val more: Comparison = Different(false)
  val less: Comparison = Different(true)
  val same: Comparison = Same

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