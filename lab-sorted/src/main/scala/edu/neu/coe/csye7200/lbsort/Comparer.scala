package edu.neu.coe.csye7200.lbsort

import scala.language.{implicitConversions, postfixOps}

trait Comparer[T] extends (((T, T)) => Comparison) {
  self =>

  //noinspection ConvertExpressionToSAM
  def toOrdering: Ordering[T] = /*SOLUTION*/ new Ordering[T]() {
    def compare(x: T, y: T): Int = self(x, y).toInt
  } /*END*/

  def >(tt: (T, T)): Boolean = /*SOLUTION*/ self(tt).flip().getOrElse(false) /*END*/

  def <(tt: (T, T)): Boolean = self(tt)().getOrElse(false)

  def ==(tt: (T, T)): Boolean = /*SOLUTION*/ self(tt)().isEmpty /*END*/

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

  implicit val intComparer: Comparer[Int] = Ordering[Int]
  // what should follow this comment?
  /*SOLUTION*/ implicit val strComparer: Comparer[String] = Ordering[String] /*END*/

  implicit def convert[T](x: Ordering[T]): Comparer[T] = (tt: (T, T)) => Comparison(x.compare(tt._1, tt._2))
}
