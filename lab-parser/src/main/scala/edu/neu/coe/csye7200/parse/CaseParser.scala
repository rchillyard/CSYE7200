/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.parse

import com.phasmid.laScala.values.Scalar
import com.phasmid.laScala.{Prefix, Renderable}
import edu.neu.coe.csye7200.parse.CaseParser.{rAnd, rBetween, rCase, rElse, rEnd, rOr, rThen, rWhen, sAnd, sOr}
import edu.neu.coe.csye7200.parse.SQLParser.rLimit

import scala.language.implicitConversions
import scala.util._
import scala.util.matching.Regex

/**
  * This class defines a parser for Strings which represent a (possibly nested) Case Clause.
  *
  */
abstract class AbstractCaseParser extends AbstractFunctionParser {

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
      case this.Failure(x, _) => parseFailure(s, "caseClause", x)
      case this.Error(x, _) => parseFailure(s, "caseClause", x)
    }
  }

  /**
    * The definition of the parser of a case clause
    *
    * @return a Parser of Invocation
    */
  def caseClause: Parser[Invocation] = rCase ~> rep(whenThen) ~ opt(elseClause) <~ rEnd ^^ { case xs ~ eo => InvocationCaseClause(xs, eo) }

  /**
    * The definition of the parser of a when/then clause
    *
    * @return a Parser of Invocation
    */
  def whenThen: Parser[Invocation] = rWhen ~> booleanExpression ~ (rThen ~> parameter) ^^ { case f ~ t => InvocationWhenThen(f, t) }

  /**
    * The definition of the parser of an else clause
    *
    * @return a Parser of Invocation
    */
  def elseClause: Parser[Expression] = rElse ~> parameter

  def booleanExpression: Parser[Invocation] = predicate ~ rep(booleanTerm) ^^ { case p ~ ts => InvocationBooleanExpression(p, ts) }

  def booleanTerm: Parser[BooleanTerm] = booleanOp ~ predicate ^^ { case op ~ p => BooleanTerm(BooleanOp(op), p) }

  def booleanOp: Parser[String] = rAnd | rOr | failure("boolean operator")

  def predicate: Parser[Expression] = (range | comparison | compareFunction | function | term | failure("predicate")) ^^ { case i: Invocation => Right(i); case x: Scalar => Left(x) }

  def compareFunction: Parser[Invocation] = openParen ~> booleanExpression <~ closeParen

  def comparison: Parser[Invocation] = parameter ~ opCompare ~ parameter ^^ { case p ~ c ~ q => InvocationComparison(p, c, q) }

  def range: Parser[Invocation] = (parameter <~ rBetween) ~ parameter ~ (rAnd ~> parameter) ^^ {
    case p ~ q ~ r =>
      InvocationBooleanExpression(Right(InvocationComparison(q, "<=", p)), List(BooleanTerm(And, Right(InvocationComparison(p, "<=", r)))))
  }

  def opCompare: Parser[String] = sGe | sGt | sEq | sNe1 | sNe2 | sLe | sLt | failure("opCompare")

  override def reserved: Parser[String] = super.reserved | rCase | rElse | rEnd | rThen |
    rWhen | rAnd | rOr | rLimit


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
  override def toString: String = sAnd
}

case object Or extends BooleanOp {
  override def toString: String = sOr
}

object BooleanOp {
  def apply(s: String): BooleanOp = s.toUpperCase match {
    case `sAnd` => And
    case `sOr` => Or
    case _ => throw ParserException(s"BooleanOp not recognized for $s") // CHECK
  }
}

object CaseParser extends AbstractCaseParser {
  val sAnd = "AND"

  val sOr = "OR"

  /**
    * case-independent regular expression to match CASE
    */
  val rCase: Regex = """(?i)CASE""".r

  /**
    * case-independent regular expression to match END
    */
  val rEnd: Regex = """(?i)END""".r

  /**
    * case-independent regular expression to match WHEN
    */
  val rWhen: Regex = """(?i)WHEN""".r

  /**
    * case-independent regular expression to match THEN
    */
  val rThen: Regex = """(?i)THEN""".r

  /**
    * case-independent regular expression to match ELSE
    */
  val rElse: Regex = """(?i)ELSE""".r

  /**
    * case-independent regular expression to match AND
    */
  val rAnd: Regex = (s"""(?i)$sAnd""").r

  /**
    * case-independent regular expression to match OR
    */
  val rOr: Regex = (s"""(?i)$sOr""").r

  /**
    * case-independent regular expression to match BETWEEN
    */
  val rBetween: Regex = """(?i)BETWEEN""".r
}
