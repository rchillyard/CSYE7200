/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.parse

import com.phasmid.laScala.values.Scalar

import scala.language.implicitConversions
import scala.util._
import scala.util.matching.Regex

/**
  * This class defines a parser for Strings which represent a SQL statement.
  *
  * TODO at this point, the parsing has been tested but none of the functionality has yet been tested.
  *
  * TODO implement the following elements: COUNT, etc. GROUP BY, HAVING, DISTINCT, special JOINs, UNION,
  */
class SQLParser extends CaseParser {

  /**
    * The chief method of the parser: it takes a String and returns an Invocation, wrapped in Try.
    *
    * CHECK
    *
    * @param s the string to be parsed
    * @return an Invocation, wrapped in a Try, which is the result of parsing s
    */
  def parseSQL(s: String): Try[Invocation] = {
    parseAll(select, s) match {
      case this.Success(x, _) => scala.util.Success(x)
      case this.Failure(x, _) => FunctionParser.parseFailure(s, "sql", x)
      case this.Error(x, _) => FunctionParser.parseFailure(s, "sql", x)
    }
  }

  /**
    * The definition of the parser of a case clause
    *
    * @return a Parser of Invocation
    */
  def select: Parser[Invocation] = SQLParser.sSelect ~ columns ~ SQLParser.sFrom ~ identifier ~ opt(whereClause) ~ opt(limitClause) ~ opt(orderByClause) ^^ {
    case _ ~ xs ~ _ ~ p ~ wo ~ lo ~ oo => InvocationSelect(xs, p, wo, lo, oo)
  }

  /**
    * The definition of the parser of a column list (or "*")
    *
    * @return a Parser of List[Invocation] which is empty if the columns were defined as "*"
    */
  def columns: Parser[List[Invocation]] = projection ^^ { case xs: List[_] => xs.asInstanceOf[List[Invocation]]; case _ => List[Invocation]() }

  /**
    * The definition of a "projection", tha part of the SELECT which defines what will be extracted (projected) from the table
    *
    * @return a Parser[Any]
    */
  def projection: Parser[Any] =
    """\*""".r | repsep(aliasedColumn, ",") | failure("projection")

  /**
    * The definition of the parser of a (potentially) aliased column
    *
    * @return a Parser of Invocation
    */
  def aliasedColumn: Parser[Invocation] = pseudoColumn ~ opt(alias) ^^ { case c ~ eo => InvocationColumn(c, eo) }

  /**
    * The definition of the parser of a pseudo=column
    *
    * @return a Parser of Invocation
    */
  def pseudoColumn: Parser[Invocation] = (identifier | caseClause | failure("pseudoColumn")) ^^ { case i: Invocation => i; case t: Scalar => InvocationP(Left(t)) }

  /**
    * The definition of the parser of an alias
    *
    * @return a Parser of Scalar
    */
  def alias: Parser[Scalar] = SQLParser.sAs ~> identifier

  /**
    * The definition of the parser of a where clause
    *
    * @return a Parser of Invocation
    */
  def whereClause: Parser[Invocation] = SQLParser.sWhere ~> booleanExpression

  /**
    * The definition of the parser of a limit clause
    *
    * @return a Parser of Scalar
    */
  def limitClause: Parser[Scalar] = SQLParser.sLimit ~> term

  /**
    * The definition of the parser of a limit clause
    *
    * @return a Parser of Scalar
    */
  def orderByClause: Parser[Scalar] = SQLParser.sOrderBy ~> identifier

  override def reserved: Parser[String] = super.reserved | SQLParser.sLimit | SQLParser.sOrderBy | SQLParser.sAs | SQLParser.sFrom | SQLParser.sWhere | SQLParser.sSelect
}

object SQLParser {
  val sSelect: Regex = """(?i)SELECT""".r
  val sAs: Regex = """(?i)AS""".r
  val sFrom: Regex = """(?i)FROM""".r
  val sWhere: Regex = """(?i)WHERE""".r
  val sLimit: Regex = """(?i)LIMIT""".r
  val sOrderBy: Regex = """(?i)ORDER BY""".r
}
