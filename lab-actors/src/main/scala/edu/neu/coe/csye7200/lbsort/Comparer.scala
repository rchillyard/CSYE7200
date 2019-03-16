package edu.neu.coe.csye7200.lbsort

import scala.language.{implicitConversions, postfixOps}

/**
  * Comparer Trait.
  *
  * NOTE: this is not exactly like the Comparer class in lab-sorted because this has to compile with 2.11
  *
  * @tparam T the underlying type to be compared.
  */
trait Comparer[T] extends (Comparands[T] => Comparison) {
  self =>

  //noinspection ConvertExpressionToSAM
  def toOrdering: Ordering[T] = ??? // TO BE IMPLEMENTED

  def >(comparands: Comparands[T]): Boolean = ??? // TO BE IMPLEMENTED

  def <(comparands: Comparands[T]): Boolean = self(comparands)().getOrElse(false)

  def same(comparands: Comparands[T]): Boolean = ??? // TO BE IMPLEMENTED

  def >=(comparands: Comparands[T]): Boolean = ! <(comparands)

  def <=(comparands: Comparands[T]): Boolean = ! >(comparands)

  def differ(comparands: Comparands[T]): Boolean = ! same(comparands)

  /**
    * Compose this Comparer with the function f: U=>T
    *
    * @param f a function U => T.
    * @tparam U the underlying type of the result.
    * @return a Comparer[U]
    */
  def compose[U](f: U => T): Comparer[U] = new Comparer[U] {
    def apply(uc: Comparands[U]): Comparison = self(uc map f)
  }

  def compose[U](uc: => Comparer[U]): Comparer[(T, U)] = new Comparer[(T, U)] {
    def apply(tUc: Comparands[(T, U)]): Comparison = self(tUc.t1._1->tUc.t2._1) orElse uc(tUc.t1._2->tUc.t2._2)
  }

  def andThen(f: Comparison => Comparison): Comparer[T] = new Comparer[T] {
    def apply(tc: Comparands[T]): Comparison = f(self(tc))
  }

  /**
    * Compose this Comparer with another Comparer of the same underlying type.
    *
    * @param o the other Comparer (lazily evaluated).
    * @return the result of applying this Comparer unless it yields Same, in which case we invoke the other Comparer.
    */
  def orElse(o: => Comparer[T]): Comparer[T] = new Comparer[T] {
    def apply(tT: (T, T)): Comparison = self(tT).orElse(o(tT))

    override def apply(tc: Comparands[T]): Comparison = self(tc) orElse o(tc)
  }

  def invert: Comparer[T] = andThen(_ flip)
}

object Comparer {

  implicit val intComparer: Comparer[Int] = Ordering[Int]
  // what should follow this comment?
  ??? // TO BE IMPLEMENTED

  implicit def convert[T](x: Ordering[T]): Comparer[T] = new Comparer[T] {
    def apply(tT: (T, T)): Comparison = Comparison(x.compare(tT._1, tT._2))

    override def apply(tc: Comparands[T]): Comparison = Comparison(x.compare(tc.t1,tc.t2))
  }
}

case class Comparands[T](t1: T, t2: T) {
  def toTuple: (T,T) = t1->t2
  def map[U](f: T=>U): Comparands[U] = Comparands(f(t1),f(t2))
}

object Comparands {
  implicit def fromTuple[T](tT: (T,T)): Comparands[T] = Comparands(tT._1,tT._2)
}

trait Comparable[T] {
  def comparands(t: T): Comparands[T]

  def compare(t: T)(implicit comparer: Comparer[T]): Comparison = comparer(comparands(t))

  def >(t: T)(implicit comparer: Comparer[T]): Boolean = compare(t).flip().getOrElse(false)

  def <(t: T)(implicit comparer: Comparer[T]): Boolean = compare(t).apply().getOrElse(false)

  def <=(t: T)(implicit comparer: Comparer[T]): Boolean = ! >(t)

  def >=(t: T)(implicit comparer: Comparer[T]): Boolean = ! <(t)

  def ==(t: T)(implicit comparer: Comparer[T]): Boolean = compare(t).apply().isEmpty

  def !=(t: T)(implicit comparer: Comparer[T]): Boolean = ! ==(t)
}
