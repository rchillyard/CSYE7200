/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.parse

import com.phasmid.laScala.values.Scalar
import edu.neu.coe.csye7200.parse.SQLParser.{rAs, rFrom, rLimit, rOrderBy, rSelect, rWhere}

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
abstract class AbstractSQLParser extends AbstractCaseParser {

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
      case this.Failure(x, _) => parseFailure(s, "sql", x)
      case this.Error(x, _) => parseFailure(s, "sql", x)
    }
  }

  /**
    * The definition of the parser of a case clause
    *
    * @return a Parser of Invocation
    */
  def select: Parser[Invocation] = selectClause ~ fromClause ~ opt(whereClause) ~ opt(limitClause) ~ opt(orderByClause) ^^ {
    case xs ~ t ~ wo ~ lo ~ oo => InvocationSelect(xs, t, wo, lo, oo)
  }

  def fromClause: Parser[Scalar] = rFrom ~> identifier

  /**
    * The definition of the parser of a column list (or "*")
    *
    * @return a Parser of List[Invocation] which is empty if the selectClause were defined as "*"
    */
  def selectClause: Parser[List[Invocation]] = rSelect ~> projection ^^ { case xs: List[_] => xs.asInstanceOf[List[Invocation]]; case _ => List[Invocation]() }

  /**
    * The definition of a "projection", tha part of the SELECT which defines what will be extracted (projected) from the table
    *
    * @return a Parser[Any]
    */
  def projection: Parser[Any] = """\*""".r | repsep(aliasedColumn, ",") | failure("projection")

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
  def alias: Parser[Scalar] = rAs ~> identifier

  /**
    * The definition of the parser of a where clause
    *
    * @return a Parser of Invocation
    */
  def whereClause: Parser[Invocation] = rWhere ~> booleanExpression

  /**
    * The definition of the parser of a limit clause
    *
    * @return a Parser of Scalar
    */
  def limitClause: Parser[Scalar] = rLimit ~> term

  /**
    * The definition of the parser of a limit clause
    *
    * @return a Parser of Scalar
    */
  def orderByClause: Parser[Scalar] = rOrderBy ~> identifier

  override def reserved: Parser[String] = super.reserved | rLimit | rOrderBy | rAs | rFrom | rWhere | rSelect
}

object SQLParser extends AbstractSQLParser {
  /**
    * case-independent regular expression to match SELECT
    */
  val rSelect: Regex = """(?i)SELECT""".r

  /**
    * case-independent regular expression to match AS
    */
  val rAs: Regex = """(?i)AS""".r

  /**
    * case-independent regular expression to match FROM
    */
  val rFrom: Regex = """(?i)FROM""".r

  /**
    * case-independent regular expression to match WHERE
    */
  val rWhere: Regex = """(?i)WHERE""".r

  /**
    * case-independent regular expression to match LIMIT
    */
  val rLimit: Regex = """(?i)LIMIT""".r

  /**
    * case-independent regular expression to match ORDER BY
    */
  val rOrderBy: Regex = """(?i)ORDER BY""".r
}
