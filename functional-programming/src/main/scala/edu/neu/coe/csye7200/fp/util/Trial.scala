package edu.neu.coe.csye7200.fp.util

import scala.util.{Failure, Try}

/**
  * Classes which provide chaining of Try operations where each operation is (typically) a function which takes
  * the same parameter.
  * There are two distinct forms:
  * <ul><li>the Trial/Match classes where the function given is of the form V=>Try[T]</li>
  * <li>the LiftTrial/LiftMatch classes where the function given is of the form V=>T</li>
  * </ul>
  * The more general class of each form is Trial (LiftTrial) which takes any function which operates on the given parameter.
  * The other class is Match/LiftMatch which takes a PartialFunction of form Any=>Try[T] or Any=>T as appropriate.
  *
  * @author scalaprof
  */

/**
  * Trial class which can be composed leftwards or rightwards with functions (including other Trial or Match object and even LiftTrial and LiftMatch objects)
  *
  * @param f the function which, when applied to the input parameter of type V, will yield a Try[T]
  * @tparam V the type of the input parameter
  * @tparam T the underlying type of the resulting Try
  */
case class Trial[V, T](f: V => Try[T]) extends TrialBase[V, T](f)

object Trial {
  // The following method creates a null trial which can be used at the start or end
  // of a chain of functions
  def none[V, T]: Trial[V, T] = Trial.apply(_ => Failure(new Exception("null trial")))

  def lift[V, T](f: V => T): Trial[V, T] = Trial(Lift(f))
}

/**
  * Match class which can be composed leftwards or rightwards with functions (including other TrialBase object)
  *
  * @param f the partial function which, when applied to the input parameter of type Any, will yield a Try[T]
  * @tparam T the underlying type of the resulting Try
  */
case class Match[T](f: PartialFunction[Any, Try[T]]) extends TrialBase[Any, T](f)

/**
  * LiftTrial class which can be composed leftwards or rightwards with functions (including other TrialBase object).
  * Note that anywhere you can right LiftTrial(f), you could, more simply, write Trial(Lift(f))
  *
  * @param f the function which, when applied to the input parameter of type V, will yield a T
  * @tparam V the type of the input parameter
  * @tparam T the underlying type of the resulting Try
  */
case class LiftTrial[V, T](f: V => T) extends TrialBase[V, T](Lift(f))

/**
  * LiftMatch class which can be composed leftwards or rightwards with functions (including other TrialBase object)
  * Note that anywhere you can right LiftMatch(f), you could, in theory, write Trial(Lift(f)) but in practice it can
  * be tricky so this definition remains for more convenience.
  *
  * @param f the partial function which, when applied to the input parameter of type Any, will yield a T
  * @tparam T the underlying type of the resulting Try
  */
case class LiftMatch[T](f: PartialFunction[Any, T]) extends TrialBase[Any, T](Lift(f))

case class Lift[V, T](f: V => T) extends (V => Try[T]) {
  def apply(v: V): Try[T] = Try(f(v))
}

/**
  * Abstract base class for Trial, Match, LiftTrial and LiftMatch
  *
  * @param f the function which, when applied to the input parameter of type V, will yield a Try[T]
  * @tparam V the type of the input parameter
  * @tparam T the underlying type of the resulting Try
  */
abstract class TrialBase[V, T](f: V => Try[T]) extends (V => Try[T]) {
  def apply(v: V): Try[T] = f(v)

  private def orElse(f: V => Try[T], g: V => Try[T]): V => Try[T] = { v => f(v) orElse g(v) }

  def |:(g: V => Try[T]): Trial[V, T] = Trial(orElse(g, f))

  def :|(g: V => Try[T]): Trial[V, T] = Trial(orElse(f, g))

  def ^:(g: V => T): Trial[V, T] = Trial(orElse(Lift(g), f))

  def :^(g: V => T): Trial[V, T] = Trial(orElse(f, Lift(g)))
}
