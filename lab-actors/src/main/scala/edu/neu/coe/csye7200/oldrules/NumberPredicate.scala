package edu.neu.coe.csye7200.oldrules

import scala.util._

/**
  * @author robinhillyard
  */
case class NumberPredicate(variable: String, operator: Operator[Double], value: Double) extends Predicate {

  def apply(candidate: Candidate): Try[Boolean] = candidate(variable) match {
    case Some(x) => Try {
      operate(x, operator, value)
    } match {
      case Success(v) => Success(v)
      case Failure(f) => Failure(f)
    }
    case _ => Failure(new Exception(s"variable $variable not found in $candidate"))
  }

  // CONSIDER Moving this into Operator class
  def operate(x: Any, operator: Operator[Double], value: Double): Boolean = {
    x match {
      case y: Double => operator(y, value)
      case y: Int => operator(y, value)
      case y: String => operator(y.toDouble, value)
      case _ => throw new Exception(s"variable $variable cannot be for operator $operator")
    }
  }
}

object NumberPredicate {
  def apply(variable: String, operator: String, value: Double): NumberPredicate =
    new NumberPredicate(variable, Operator.createNumeric(operator), value)

  def apply(variable: String, operator: Operator[Double], value: String): NumberPredicate =
    new NumberPredicate(variable, operator, value.toDouble)

  def apply(variable: String, operator: String, value: String): NumberPredicate =
    apply(variable, Operator.createNumeric(operator), value)

  def apply(predicate: String): NumberPredicate = {
    val rPredicate = """^\s*(\w+)\s*([=<>]{1,2})\s*(-?[0-9]+\.?[0-9]*)\s*$""".r
    predicate match {
      case rPredicate(v, o, n) => apply(v, o, n)
      case _ => throw new Exception(s"predicate: $predicate is malformed")
    }
  }
}
