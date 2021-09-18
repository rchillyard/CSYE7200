/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.parse

import com.phasmid.laScala.values.Scalar
import com.phasmid.laScala.{Prefix, Renderable}

import scala.language.implicitConversions
import scala.util._
import scala.util.matching.Regex

/**
  * This class defines a parser for Strings which represent a (possibly nested) Case Clause.
  *
  */
class CaseParser extends FunctionParser {

  /**
    * The chief method of the parser: it takes a String and returns an Invocation, wrapped in Try.
    *
    * @param s the string to be parsed
    * @return an Invocation, wrapped in a Try, which is the result of parsing s
    */
  def parseCaseClause(s: String): Try[Invocation] = {
    parseAll(caseClause, s) match {
      case this.Success(x, _) => scala.util.Success(x)
      // CHECK...
      case this.Failure(x, _) => FunctionParser.parseFailure(s, "caseClause", x)
      case this.Error(x, _) => FunctionParser.parseFailure(s, "caseClause", x)
    }
  }

  /**
    * The definition of the parser of a case clause
    *
    * @return a Parser of Invocation
    */
  def caseClause: Parser[Invocation] = CaseParser.sCase ~> rep(whenThen) ~ opt(elseClause) <~ CaseParser.sEnd ^^ { case xs ~ eo => InvocationCaseClause(xs, eo) }

  /**
    * The definition of the parser of a when/then clause
    *
    * @return a Parser of Invocation
    */
  def whenThen: Parser[Invocation] = CaseParser.sWhen ~ booleanExpression ~ CaseParser.sThen ~ parameter ^^ { case _ ~ f ~ _ ~ t => InvocationWhenThen(f, t) }

  /**
    * The definition of the parser of an else clause
    *
    * @return a Parser of Invocation
    */
  def elseClause: Parser[Expression] = CaseParser.sElse ~> parameter

  def booleanExpression: Parser[Invocation] = predicate ~ rep(booleanTerm) ^^ { case p ~ ts => InvocationBooleanExpression(p, ts) }

  def booleanTerm: Parser[BooleanTerm] = booleanOp ~ predicate ^^ { case op ~ p => BooleanTerm(BooleanOp(op), p) }

  def booleanOp: Parser[String] = CaseParser.sAnd | CaseParser.sOr | failure("boolean operator")

  def predicate: Parser[Expression] = (range | comparison | compareFunction | function | term | failure("predicate")) ^^ { case i: Invocation => Right(i); case x: Scalar => Left(x) }

  def compareFunction: Parser[Invocation] = openParen ~> booleanExpression <~ closeParen

  def comparison: Parser[Invocation] = parameter ~ opCompare ~ parameter ^^ { case p ~ c ~ q => InvocationComparison(p, c, q) }

  def range: Parser[Invocation] = parameter ~ CaseParser.sBetween ~ parameter ~ CaseParser.sAnd ~ parameter ^^ {
    case p ~ _ ~ q ~ _ ~ r =>
      InvocationBooleanExpression(Right(InvocationComparison(q, "<=", p)), List(BooleanTerm(And, Right(InvocationComparison(p, "<=", r)))))
  }

  def opCompare: Parser[String] = sGe | sGt | sEq | sNe1 | sNe2 | sLe | sLt | failure("opCompare")

  override def reserved: Parser[String] = super.reserved | CaseParser.sCase | CaseParser.sElse | CaseParser.sEnd | CaseParser.sThen |
    CaseParser.sWhen | CaseParser.sAnd | CaseParser.sOr | SQLParser.sLimit


  val sGt = ">"
  val sGe = ">="
  val sEq = "="
  val sNe1 = "<>"
  val sNe2 = "!="
  val sLe = "<="
  val sLt = "<"
}

case class BooleanTerm(op: BooleanOp, e: Expression) extends Renderable {
  def render(indent: Int)(implicit tab: Int => Prefix): String = op.toString + " " + InvocationBase.renderExpression(e, indent + 1)
}

trait BooleanOp

case object And extends BooleanOp {
  override def toString: String = "AND"
}

case object Or extends BooleanOp {
  override def toString: String = "OR"
}

object BooleanOp {
  def apply(s: String): BooleanOp = s.toUpperCase match {
    case "AND" => And
    case "OR" => Or
    case _ => throw ParserException(s"BooleanOp not recognized for $s") // CHECK
  }
}

object CaseParser {
  val sCase: Regex = """(?i)CASE""".r
  val sEnd: Regex = """(?i)END""".r
  val sWhen: Regex = """(?i)WHEN""".r
  val sThen: Regex = """(?i)THEN""".r
  val sElse: Regex = """(?i)ELSE""".r
  val sAnd: Regex = """(?i)AND""".r
  val sOr: Regex = """(?i)OR""".r
  val sBetween: Regex = """(?i)BETWEEN""".r
}
