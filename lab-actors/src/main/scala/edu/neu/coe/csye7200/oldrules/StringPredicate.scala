package edu.neu.coe.csye7200.oldrules

import scala.util._

/**
  * @author robinhillyard
  */
case class StringPredicate(variable: String, operator: Operator[String], value: String) extends Predicate {

  def apply(candidate: Candidate): Try[Boolean] = candidate(variable) match {
    case Some(x) => Try {
      operator(x.toString, value)
    } match {
      case Success(v) => Success(v)
      case Failure(f) => Failure(f)
    }
    case _ => Failure(new Exception(s"variable $variable not found in $candidate"))
  }
}

object StringPredicate {
  def apply(variable: String, operator: String, value: String): StringPredicate =
    new StringPredicate(variable, Operator.createText(operator), value)
}
