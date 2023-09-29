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

  //noinspection ConvertExpressionToSAM
  def toOrdering: Ordering[T] =
// TO BE IMPLEMENTED 



???

  def >(tt: (T, T)): Boolean = {
// TO BE IMPLEMENTED 

???
  }

  def <(tt: (T, T)): Boolean = self(tt)().getOrElse(false)

  def ==(tt: (T, T)): Boolean = {
// TO BE IMPLEMENTED 

???
  }

  def >=(tt: (T, T)): Boolean = ! <(tt)

  def <=(tt: (T, T)): Boolean = ! >(tt)

  def !=(tt: (T, T)): Boolean = ! ==(tt)

  def unMap[U](f: U => T): Comparer[U] = (uU: (U, U)) => self((f(uU._1), f(uU._2)))

  def compose[U](uc: => Comparer[U]): Comparer[(T, U)] = (tut: ((T, U), (T, U))) => self(tut._1._1 -> tut._2._1) orElse uc(tut._1._2 -> tut._2._2)

  def map(f: Comparison => Comparison): Comparer[T] = (tt: (T, T)) => f(self(tt))

  /**
    * Compose this Comparer with another Comparer of the same underlying type.
    *
    * @param o the other Comparer (lazily evaluated).
    * @return the result of applying this Comparer unless it yields Same, in which case we invoke the other Comparer.
    */
  def orElse(o: => Comparer[T]): Comparer[T] = (tt: (T, T)) => self(tt).orElse(o(tt))

  def invert: Comparer[T] = map(_ flip)
}

object Comparer {

  // NOTE: the reason this is so simple is that there is an implicit converter (below) which converts an Ordering[T] into a Comparer[T].
  implicit val intComparer: Comparer[Int] = Ordering[Int]
  // what should follow this comment?

  // TO BE IMPLEMENTED 

  // STUB
  // END

  implicit def convert[T](x: Ordering[T]): Comparer[T] = (tt: (T, T)) => Comparison(x.compare(tt._1, tt._2))
}