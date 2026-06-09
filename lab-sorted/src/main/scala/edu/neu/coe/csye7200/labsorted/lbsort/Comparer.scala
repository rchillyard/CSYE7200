package edu.neu.coe.csye7200.labsorted.lbsort

import scala.language.{implicitConversions, postfixOps}

/**
 * This trait defines methods to aid in comparing instances of type T.
 * The apply method takes a tuple of two Ts and returns a Comparison.
 *
 * For much more comprehensive comparison library, please see https://github.com/rchillyard/Comparer.
 *
 * @tparam T the type of the comparands.
 */
trait Comparer[T] extends (((T, T)) => Comparison) {
  self =>

  /**
   * Converts the current `Comparer` instance into a `scala.math.Ordering` for use in standard Scala sorting.
   *
   * @return an `Ordering[T]` instance that compares elements of type `T` based on the logic of this `Comparer`.
   */
  //noinspection ConvertExpressionToSAM
  def toOrdering: Ordering[T] =
    // TO BE IMPLEMENTED  (hint: this Comparer can be referenced as self)
    ???

  /**
   * Determines if the first element in the tuple is greater than the second element
   * according to the logic of this `Comparer`.
   *
   * @param tt a tuple of two elements of type T to be compared.
   * @return true if the first element is greater than the second element, false otherwise.
   */
  def >(tt: (T, T)): Boolean =
    // TO BE IMPLEMENTED 
    ???

  /**
   * Determines if the first element in the tuple is less than the second element
   * according to the logic of this `Comparer`.
   *
   * @param tt a tuple of two elements of type T to be compared.
   * @return true if the first element is less than the second element, false if it is not, or false if the comparison cannot be determined.
   */
  def <(tt: (T, T)): Boolean =
    self(tt)().getOrElse(false)

  /**
   * Compares a tuple of two elements according to the logic of this `Comparer` to determine if they are equal.
   *
   * @param tt a tuple containing two elements of type `T` to be compared.
   * @return true if the two elements are considered equal based on the logic of this `Comparer`, false otherwise.
   */
  def ==(tt: (T, T)): Boolean = {
    // TO BE IMPLEMENTED 
        ???
  }

  /**
   * Determines if the first element in the tuple is greater than or equal to the second element
   * according to the logic of this `Comparer`.
   * NOTE this references the `<` method so beware of infinite recursion.
   *
   * @param tt a tuple of two elements of type T to be compared.
   * @return true if the first element is greater than or equal to the second element, false otherwise.
   */
  def >=(tt: (T, T)): Boolean = ! <(tt)

  /**
   * Determines if the first element in the tuple is less than or equal to the second element
   * according to the logic of this `Comparer`.
   * NOTE this references the `>` method so beware of infinite recursion.
   *
   * @param tt a tuple of two elements of type T to be compared.
   * @return true if the first element is less than or equal to the second element, false otherwise.
   */
  def <=(tt: (T, T)): Boolean = ! >(tt)

  /**
   * Determines if two elements in a tuple are unequal, according to the logic of this `Comparer`.
   *
   * @param tt a tuple of two elements of type T to be compared for inequality.
   * @return true if the two elements are not equal, false otherwise.
   */
  def !=(tt: (T, T)): Boolean = ! ==(tt)

  /**
   * This method is essentially a `lens` function based on this `Comparer` instance.
   *
   * @param f a transformation function that converts an input of type `U` to type `T`.
   * @return a new `Comparer[U]` that compares elements of type `U` by mapping them to type `T` using the function `f`.
   */
  def unMap[U](f: U => T): Comparer[U] = (uU: (U, U)) =>
    self((f(uU._1), f(uU._2)))

  /**
   * Composes the current `Comparer` with another `Comparer` that compares elements of a different type.
   * The resulting `Comparer` operates on tuples of `T` and `U`, combining the comparison logic of both `Comparer` instances.
   *
   * @param uc a lazily evaluated `Comparer[U]` instance used for comparing the second component of the tuple.
   * @return a `Comparer[(T, U)]` that compares tuples by applying this `Comparer` to the first component
   *         and the provided `Comparer` to the second component, using `orElse` logic where applicable.
   */
  def compose[U](uc: => Comparer[U]): Comparer[(T, U)] =
    (tut: ((T, U), (T, U))) => self(tut._1._1 -> tut._2._1) orElse uc(tut._1._2 -> tut._2._2)

  /**
   * Transforms the comparison function of this comparer by applying a given function to the result of the comparison.
   *
   * @param f A function that takes a `Comparison` and returns a new `Comparison`. This function is applied to the output
   *          of the current comparison function.
   *
   * @return A new comparer where the comparison logic has been transformed by the provided function `f`.
   */
  def map(f: Comparison => Comparison): Comparer[T] =
    (tt: (T, T)) => f(self(tt))

  /**
   * Compose this Comparer with another Comparer of the same underlying type.
   *
   * @param tc the other Comparer (lazily evaluated).
   * @return the result of applying this Comparer unless it yields Same, in which case we invoke the other Comparer.
   */
  def orElse(tc: => Comparer[T]): Comparer[T] =
    (tt: (T, T)) => self(tt).orElse(tc(tt))

  /**
   * Inverts the logic of this `Comparer`. The resulting `Comparer` will reverse the original comparison
   * results, flipping "less than" to "greater than", and vice versa, while leaving "same" unchanged.
   *
   * @return a new `Comparer[T]` that reverses the comparison logic of this `Comparer`.
   */
  def invert: Comparer[T] = map(_ flip)
}

/**
 * Companion object for the `Comparer` type, providing implicit conversions
 * and predefined instances for standard types.
 */
object Comparer {

  // NOTE: the reason this is so simple is that there is an implicit Conversion (declared below) which converts an Ordering[T] into a Comparer[T].
  given intComparer: Comparer[Int] = Ordering[Int]
  // what should follow this comment?

  // TO BE IMPLEMENTED 
    ???

  /**
   * An implicit Conversion that transforms an `Ordering[X]` into a `Comparer[X]`.
   *
   * This conversion allows the use of `Ordering[X]` instances where a `Comparer[X]` is expected.
   * The converted `Comparer` leverages the logic of the given `Ordering` to compare two elements of type `X`
   * and produces a `Comparison` result.
   * See examples above within object `Comparer`.
   *
   * @tparam X the type of the elements to be compared.
   */
  given [X]: Conversion[Ordering[X], Comparer[X]] with
    def apply(xo: Ordering[X]): Comparer[X] = {
      val f = (xo.compare _).tupled
      (xx: (X, X)) => Comparison(f(xx))
    }
}