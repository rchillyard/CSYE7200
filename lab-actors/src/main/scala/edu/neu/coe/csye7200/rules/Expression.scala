/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.rules

import scala.util.{Failure, Success, Try}

/**
  * Separated from RuleParser by scalaprof on 7/8/16 at which time evaluate and asQuotedString added.
  *
  * TODO Trait Expression which needs a better description
  */
trait Expression {
  def toRPN: List[String]

  def asString: String

  /**
    * @return Some(string) if this Expression represents a single, quoted string. Otherwise, None.
    */
  def asQuotedString: Option[String]

  /**
    * Evaluate this expression as either an X, a String or (failing) a Throwable
    *
    * @tparam X the underlying type of the Valuable result
    * @return Either a Throwable or Either a String or an X
    */
  def evaluate[X: Valuable](implicit lookup: String => Option[X]): Try[Either[String, X]] =
    asQuotedString match {
      case Some(s) => Success(Left(s))
      case None => RPN(toRPN).evaluate match {
        case Success(x) => Success(Right(x))
        case Failure(t) => Failure(t)
      }
    }
}
