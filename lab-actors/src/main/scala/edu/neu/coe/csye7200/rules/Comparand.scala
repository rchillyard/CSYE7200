/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.rules

import scala.util.Try

/**
  * This class is similar to RPN but applies to Rule right-hand-sides that are Orderable whereas RPN applies when RHS is Valuable
  * (in other words, an expression).
  *
  * @param xt the value of X wrapped in Try
  * @tparam X the underlying type
  */
case class Comparand[X: Orderable](xt: Try[X]) extends Evaluable[X] {
  def evaluate: Try[X] = xt
}

object Comparand {
  /**
    * Construct a Comparand[X} and return it as an Evaluable[X]
    *
    * @param s      the String from which the Comparand is to be built: usually the RHS of a predicate
    * @param lookup the lookup function to evaluate variables
    * @tparam X the underlying type
    * @return an Evaluable
    */
  def apply[X: Orderable](s: String)(implicit lookup: String => Option[X]): Evaluable[X] = {
    //    val n: Orderable[X] = implicitly[Orderable[X]]
    val lookupR = """\$(\w+)""".r
    s match {
      case lookupR(x) => Comparand[X](FP.optionToTry(lookup(x), new Exception(s"no value for lookup: $x")))
      case _ => Invalid(new RuleException(s"invalid token: $s"))
    }
  }

  /**
    * Construct a Comparand from w and evaluate it.
    *
    * @param w      the String to be evaluated: usually the RHS of a predicate
    * @param lookup the lookup function to evaluate variables
    * @tparam X the underlying type
    * @return a Try[X] corresponding to the value of the comparand
    */
  def evaluate[X: Orderable](w: String)(implicit lookup: String => Option[X]): Try[X] = apply(w).evaluate
}
