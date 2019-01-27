/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.rules

import scala.util.{Failure, Success, Try}

import FP._

/**
  * Trait Rule which extends ()=>Boolean. An alternative name might be TruthValue or just Truth.
  *
  * Thus the apply method for a Rule will always return true or false, with no input parameter.
  *
  * A (leaf) rule is essentially an expression of the form: variable predicate
  * where predicate is an operator followed by an expression.
  *
  * A rule can be represented for any type T, including String.
  * However, if you want to be able to evaluate the truth value of a rule, then either
  * (1) T extends Valuable, or
  * (2) an implicit map from String to T is made available
  *
  * @tparam T the underlying type of this Rule
  */
sealed trait Rule[T] extends (() => Try[Boolean]) {
  self =>

  /**
    * TODO implement this method in terms of liftTransform
    *
    * Method to transform this Rule[T] into a Rule[U].
    *
    * The primary purpose of this method is to allow a lookup to occur (the xf functions).
    * Typically, T will be String and U will be Double or Int.
    *
    * CONSIDER it would be much nicer if there were only one function parameter to this method
    *
    * @param lf T=>U function to be applied to the lhs of the rule (the variable)
    * @param rf T=>U function to be applied to the rhs of the rule (the predicate)
    * @tparam U the type of the resulting Rule
    * @return a Rule[T] which is equivalent (truth-wise) to this Rule.
    */
  def transform[U: Ordering](lf: (T) => U, rf: (T) => U): Rule[U]

  /**
    * Method to transform this Rule[T] into a Try[Rule[U]. Somewhat similar to flatMap.
    *
    * The primary purpose of this method is to allow a lookup to occur (the xf functions).
    * Typically, T will be String and U will be Double or Int.
    *
    * CONSIDER it would be much nicer if there were only one function parameter to this method
    *
    * @param lf T=>Try[U] function to be applied to the lhs of the rule (the variable)
    * @param rf T=>Try[U] function to be applied to the rhs of the rule (the predicate)
    * @tparam U the type of the resulting Rule
    * @return a Try[Rule[T] which is equivalent (truth-wise) to this Rule.
    **/
  def liftTransform[U: Ordering](lf: (T) => Try[U], rf: (T) => Try[U]): Try[Rule[U]]

  /**
    * Conjunctive combination of self with another Rule.
    *
    * Associates to the right.
    *
    * Self will not be evaluated if c evaluates as false
    *
    * @param r the other Rule
    * @return a Rule which is the conjunctive (and) combination of r with self
    */
  def &:(r: => Rule[T]): Rule[T] = new AndRule(r, self)

  /**
    * Disjunctive combination of self with another Rule.
    *
    * Associates to the right.
    *
    * Self will not be evaluated if r evaluates as true
    *
    * @param r the other Rule
    * @return a Rule which is the disjunctive (or) combination of r with self
    */
  def |:(r: => Rule[T]): Rule[T] = new OrRule(r, self)

  /**
    * Conjunctive combination of self with another Rule.
    *
    * Associates to the left.
    *
    * @param r the other Rule (r will not be evaluated if self evaluates as false)
    * @return a Rule which is the conjunctive (and) combination of self with r
    */
  def :&(r: => Rule[T]): Rule[T] = new AndRule(self, r)

  /**
    * Disjunctive combination of self with another Rule.
    *
    * Associates to the left.
    *
    * @param r the other Rule (r will not be evaluated if self evaluates as true)
    * @return a Rule which is the disjunctive (or) combination of self with r
    */
  def :|(r: => Rule[T]): Rule[T] = new OrRule(self, r)
}

abstract class BaseRule[T](name: String) extends Rule[T] {
  override def toString: String = name
}

/**
  * "AndRule" sub-class of Rule giving the conjunction of r1 and r2.
  * Not a case class so that we can have a call-by-name parameter
  *
  * @param r1 the rule which is always evaluated
  * @param r2 the rule which may not be evaluated
  * @tparam T the underlying type of this Rule
  */
class AndRule[T](r1: Rule[T], r2: => Rule[T]) extends BaseRule[T](s"$r1 & $r2") {
  def transform[U: Ordering](f: (T) => U, g: (T) => U): Rule[U] = new AndRule[U](r1 transform(f, g), r2 transform(f, g))

  def apply(): Try[Boolean] = map2(r1(), r2())(_ && _)

  /**
    * Method to transform this Rule[T] into a Try[Rule[U]. Somewhat similar to flatMap.
    *
    * The primary purpose of this method is to allow a lookup to occur (the xf functions).
    * Typically, T will be String and U will be Double or Int.
    *
    * CONSIDER it would be much nicer if there were only one function parameter to this method
    *
    * @param lf T=>Try[U] function to be applied to the lhs of the rule (the variable)
    * @param rf T=>Try[U] function to be applied to the rhs of the rule (the predicate)
    * @tparam U the type of the resulting Rule
    * @return a Try[Rule[T] which is equivalent (truth-wise) to this Rule.
    **/
  def liftTransform[U: Ordering](lf: (T) => Try[U], rf: (T) => Try[U]): Try[AndRule[U]] =
    for (s1 <- r1 liftTransform(lf, rf); s2 <- r2 liftTransform(lf, rf)) yield new AndRule[U](s1, s2)
}

/**
  * "OrRule" sub-class of Rule giving the disjunction of r1 and r2.
  * Not a case class so that we can have a call-by-name parameter
  *
  * @param r1 the rule which is always evaluated
  * @param r2 the rule which may not be evaluated
  * @tparam T the underlying type of this Rule
  */
class OrRule[T](r1: Rule[T], r2: => Rule[T]) extends BaseRule[T](s"($r1 | $r2)") {
  def transform[U: Ordering](f: (T) => U, g: (T) => U): Rule[U] = new OrRule[U](r1 transform(f, g), r2 transform(f, g))

  def apply(): Try[Boolean] = map2(r1(), r2())(_ || _)

  /**
    * Method to transform this Rule[T] into a Try[Rule[U]. Somewhat similar to flatMap.
    *
    * The primary purpose of this method is to allow a lookup to occur (the xf functions).
    * Typically, T will be String and U will be Double or Int.
    *
    * CONSIDER it would be much nicer if there were only one function parameter to this method
    *
    * @param lf T=>Try[U] function to be applied to the lhs of the rule (the variable)
    * @param rf T=>Try[U] function to be applied to the rhs of the rule (the predicate)
    * @tparam U the type of the resulting Rule
    * @return a Try[Rule[T] which is equivalent (truth-wise) to this Rule.
    **/
  def liftTransform[U: Ordering](lf: (T) => Try[U], rf: (T) => Try[U]): Try[OrRule[U]] =
    for (s1 <- r1 liftTransform(lf, rf); s2 <- r2 liftTransform(lf, rf)) yield new OrRule[U](s1, s2)
}

/**
  * Bound predicate sub-class of Rule giving the result of applying a parameter to a predicate.
  * Not a case class so that we can have a call-by-name parameter
  *
  * @param t the parameter
  * @param p the Predicate
  */
class BoundPredicate[T](t: => T, p: => Predicate[T]) extends BaseRule[T](s"($t $p)") {
  self =>
  def transform[U: Ordering](f: (T) => U, g: (T) => U): Rule[U] = new BoundPredicate[U](f(t), p map g)

  def apply(): Try[Boolean] = p(t)

  /**
    * Method to transform this Rule[T] into a Try[Rule[U]. Somewhat similar to flatMap.
    *
    * The primary purpose of this method is to allow a lookup to occur (the xf functions).
    * Typically, T will be String and U will be Double or Int.
    *
    * CONSIDER it would be much nicer if there were only one function parameter to this method
    *
    * @param lf T=>Try[U] function to be applied to the lhs of the rule (the variable)
    * @param rf T=>Try[U] function to be applied to the rhs of the rule (the predicate)
    * @tparam U the type of the resulting Rule
    * @return a Try[Rule[T] which is equivalent (truth-wise) to this Rule.
    **/
  def liftTransform[U: Ordering](lf: (T) => Try[U], rf: (T) => Try[U]): Try[BoundPredicate[U]] =
    for (t1 <- lf(t); p1 <- p tryMap rf) yield new BoundPredicate[U](t1, p1)
}

/**
  * This sub-class of Rule simply yields a fixed boolean expression
  *
  * @param b the fixed boolean expression
  * @tparam T the underlying type of this Rule
  */
case class Truth[T](b: Boolean) extends BaseRule[T](s"$b") {
  def transform[U: Ordering](f: (T) => U, g: (T) => U): Rule[U] = Truth(b).asInstanceOf[Rule[U]]

  def apply(): Try[Boolean] = Success(b)

  /**
    * Method to transform this Rule[T] into a Try[Rule[U]. Somewhat similar to flatMap.
    *
    * The primary purpose of this method is to allow a lookup to occur (the xf functions).
    * Typically, T will be String and U will be Double or Int.
    *
    * CONSIDER it would be much nicer if there were only one function parameter to this method
    *
    * @param lf T=>Try[U] function to be applied to the lhs of the rule (the variable)
    * @param rf T=>Try[U] function to be applied to the rhs of the rule (the predicate)
    * @tparam U the type of the resulting Rule
    * @return a Try[Rule[T] which is equivalent (truth-wise) to this Rule.
    **/
  def liftTransform[U: Ordering](lf: (T) => Try[U], rf: (T) => Try[U]) = Success(Truth(b).asInstanceOf[Rule[U]])
}

case class InvalidRule[T](x: Throwable) extends BaseRule[T](s"invalid: $x") {
  def transform[U: Ordering](f: (T) => U, g: (T) => U): Rule[U] = InvalidRule(x).asInstanceOf[Rule[U]]

  def apply(): Try[Boolean] = Failure(x)

  /**
    * Method to transform this Rule[T] into a Try[Rule[U]. Somewhat similar to flatMap.
    *
    * The primary purpose of this method is to allow a lookup to occur (the xf functions).
    * Typically, T will be String and U will be Double or Int.
    *
    * CONSIDER it would be much nicer if there were only one function parameter to this method
    *
    * @param lf T=>Try[U] function to be applied to the lhs of the rule (the variable)
    * @param rf T=>Try[U] function to be applied to the rhs of the rule (the predicate)
    * @tparam U the type of the resulting Rule
    * @return a Try[Rule[T] which is equivalent (truth-wise) to this Rule.
    **/
  def liftTransform[U: Ordering](lf: (T) => Try[U], rf: (T) => Try[U]) = Success(InvalidRule(x).asInstanceOf[Rule[U]])
}

object Rule {
  def convertFromStringRuleToValuableRule[X: Valuable](rt: Try[Rule[String]], lookup: String => Option[X]): Try[Rule[X]] = {
    val stringToTriedX: (String) => Try[X] = { s => FP.optionToTry(lookup(s), new RuleException(s"cannot lookup value for $s")) }
    implicit val f: String => Option[X] = lookup
    val evaluateExpression: (String) => Try[X] = { s => RPN.evaluate[X](s) }
    for (r <- rt; z <- r liftTransform(stringToTriedX, evaluateExpression)) yield z
  }

  def convertFromStringRuleToOrderableRule[X: Orderable](rt: Try[Rule[String]], lookup: String => Option[X])(implicit pattern: String): Try[Rule[X]] = {
    val stringToTriedX: (String) => Try[X] = { s => FP.optionToTry(lookup(s), new RuleException(s"cannot lookup value for $s")) }
    implicit val f: String => Option[X] = lookup
    implicit val pattern: String = ""
    val evaluateExpression: (String) => Try[X] = { s => Comparand.evaluate[X](s) }
    for (r <- rt; z <- r liftTransform(stringToTriedX, evaluateExpression)) yield z
  }

  def lift[T, U](f: T => U): T => Try[U] = { t => Try(f(t)) }
}

class RuleException(s: String) extends Exception(s"rule problem: $s")
