/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.parse

import com.phasmid.laScala.values.{DateScalar, QuotedStringScalar, Scalar}
import edu.neu.coe.csye7200.parse.FunctionParser.{rConcat, rIn, rIsNull, rLike, rNot}

import scala.language.implicitConversions
import scala.util._
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

/**
  * This class defines a parser for Strings which represent a (possibly nested) function call.
  *
  * The string is expected to be extracted from a SQL statement and, as such, is not expected to contain any double quotes.
  * It would be easy enough to support double quotes, but for now, they are not supported.
  *
  */
abstract class AbstractFunctionParser extends JavaTokenParsers {

  /**
    * The chief method of the parser: it takes a String and returns an Invocation, wrapped in Try.
    *
    * @param s the string to be parsed
    * @return an Invocation, wrapped in a Try, which is the result of parsing s
    */
  def parseFunctionCall(s: String): Try[Invocation] = parseAll(function, s) match {
    case this.Success(x, _) => scala.util.Success(x)
    // CHECK ...
    case this.Failure(x, _) => parseFailure(s, "function call", x)
    case this.Error(x, _) => parseFailure(s, "function call", x)
  }

  def parseFailure[X](s: String, e: String, x: String): Try[X] = scala.util.Failure(ParserException(s"""unable to parse "$s" as a $e: $x""")) // CHECK

  /**
    * The definition of the parser of a function invocation
    *
    * @return a Parser of Invocation
    */
  def function: Parser[Invocation] = pn1 | fp | fPn | pf | p | failure("function")

  /**
    * The definition of the parser of a parameter set
    *
    * @return a Parser of List[Expression]
    */
  def parameterSet: Parser[Seq[Expression]] = openParen ~> repsep(parameter, comma) <~ closeParen

  /**
    * The definition of the parser of a function in the form of (p1,p2,...pN) where the list should be treated as a single parameter to another Invocation
    * For example: ('UK','US','FR')
    *
    * @return a Parser of Invocation
    */
  def pn1: Parser[Invocation] = pn1Identifier ~ parameterSet ^^ {
    case f ~ ps => InvocationFPn(f, List(Right(InvocationPn1(ps.toList))))
  }

  /**
    * The definition of the parser of a function in the form of f(p1,p2,...pN).
    *
    * @return a Parser of Invocation
    */
  def fPn: Parser[Invocation] = identifier ~ parameterSet ^^ {
    case f ~ ps => InvocationFPn(f.render(), ps.toList)
  }

  /**
    * The definition of the parser of a function in the form of p0 f.
    *
    * @return a Parser of Invocation
    */
  def pf: Parser[Invocation] = nonfunction ~ function ^^ {
    case p ~ f => InvocationPF(p, f)
  }

  /**
    * The definition of the parser of a function in the form of f p.
    * For example: NOT TRUE
    *
    * @return a Parser of Invocation
    */
  def fp: Parser[Invocation] = fpIdentifier ~ parameter ^^ {
    case f ~ p => InvocationFP(f, p)
  }

  /**
    * The definition of the parser of a parenthetical expression.
    *
    * @return a Parser of Invocation
    */
  def p: Parser[Invocation] = openParen ~> parameter <~ closeParen ^^ InvocationP

  /**
    * The definition of a Parser for a String which defines a pn1-type Parser (see above).
    * The only example I can think of for now is "IN". I'd like to generalize it but it messes up the rest of the parsing.
    *
    * @return Parser[String]
    */
  def pn1Identifier: Parser[String] = rIn | rConcat | failure("pn1Identifier")

  /**
    * The definition of a Parser for a String which defines a f-type Parser (see above).
    *
    * @return Parser[String]
    */
  def fpIdentifier: Parser[String] = rNot | "!" | "==" | rLike | failure("fpIdentifier")

  /**
    * The definition of a Parser for a String which defines a variable.
    *
    * @return Parser[String]
    */
  def variable: Parser[Invocation] = not(reserved) ~> """[\w._]+""".r ^^ InvocationLookup

  def nonfunction: Parser[Expression] = (term | variable | failure("nonfunction")) ^^ {
    case i@InvocationBase(_, _) => Right(i)
    case s@Scalar(_) => Left(s)
  }

  /**
    * The definition of the parser of a function parameter: either a term (a Scalar) or a function
    *
    * @return a Parser of Either[Scalar, Invocation]
    */
  def parameter: Parser[Expression] = (function | nonfunction | failure("parameter")) ^^ {
    case i@InvocationBase(_, _) => Right(i)
    case x: Expression@unchecked => x
  }

  /**
    * The definition of the parser of a term (a Scalar).
    * Provided it is in quotes, this parser will recognize something like 20170228 as a date.
    * However, the "normal" date parser mechanism is via the date parser (see below)
    *
    * @return a Parser of Scalar
    */
  def term: Parser[Scalar] = not(reserved) ~> (singleQuote ~ quotedString ~ singleQuote | date | number | boolean | failure("term")) ^^ {
    case _ ~ s ~ _ =>
      implicit val pattern: String = "yyyyMMdd"
      val dy = Try(DateScalar(s.toString))
      dy match {
        case scala.util.Success(d) => d
        case _ => QuotedStringScalar(s.toString)
      }
    case x: Int => Scalar(x)
    case x: Boolean => Scalar(x)
    case x: IsoDate => DateScalar(x.y, x.m, x.d)
    //    case s => Scalar(s.toString)
  }

  /**
    * The definition of the parser of a number (an Int)
    *
    * @return a Parser of Int
    */
  def number: Parser[Int] = wholeNumber ^^ (_.toInt)

  /**
    * The definition of the parser of a truth value (a Boolean)
    *
    * @return a Parser of Boolean
    */
  def boolean: Parser[Boolean] =
    """(?i)false|true""".r ^^ (_.toBoolean)

  /**
    * The definition of a parser of a date string
    *
    * NOTE: we cannot read an all-digit UK traditional date because it will preferentially match the US style first.
    *
    * @return a Parser of IsoDate
    */
  def date: Parser[IsoDate] = (usDate | ukTradDate | isoDate | failure("date")) ^^ IsoDate.apply

  /**
    * Parser for date in ISO sequence (in decreasing order of significance)
    *
    * @return a Parser of String
    *
    *         // XXX why can't we use <~ and ~> operators here?
    */
  def isoDate: Parser[String] = (year ~ ds ~ month ~ ds ~ day | shortYear ~ ds ~ month ~ ds ~ day | failure("isoDate")) ^^ { case y ~ _ ~ m ~ _ ~ d => dateString(y, m, d) }

  /**
    * Parser for date in (traditional) British sequence (in increasing order of significance)
    *
    * @return a Parser of String
    */
  def ukTradDate: Parser[String] = (day ~ ds ~ month ~ ds ~ year | day ~ ds ~ month ~ ds ~ shortYear | failure("ukTradDate")) ^^ { case d ~ _ ~ m ~ _ ~ y => dateString(y, m, d) }

  /**
    * Parser for date in US sequence (in no particular order of significance)
    *
    * @return a Parser of String
    */
  def usDate: Parser[String] = (month ~ ds ~ day ~ ds ~ year | month ~ ds ~ day ~ ds ~ shortYear | failure("usDate")) ^^ { case m ~ _ ~ d ~ _ ~ y => dateString(y, m, d) }

  def year: Parser[String] = """\d\d\d\d""".r

  def shortYear: Parser[String] = """\d\d""".r

  def month: Parser[String] = """\w\w\w""".r | """\d{1,2}""".r

  def day: Parser[String] = """\d{1,2}""".r

  def dateString(y: String, m: String, d: String): String = y + "-" + m + "-" + d

  def reserved: Parser[String] = rLike | rIn | rNot | rConcat | rIsNull | failure("reserved")

  private def ds = "-" | "/" | failure("ds")

  val identifier: Parser[Scalar] = not(reserved) ~ """\w+""".r ^^ { case _ ~ s => Scalar(s) }
  private val quotedString = """[^']*""".r
  val openParen = "("
  val closeParen = ")"
  private val comma = ","
  private val singleQuote = "'"
}

case class IsoDate(y: Int, m: Int, d: Int)

object IsoDate {
  def apply(y: Int, m: String, d: Int): IsoDate = Try(m.toInt) match {
    case scala.util.Success(x) => apply(y, x, d)
    case _ =>
      // month lookup table. Obviously, this only works for English strings
      val months = Seq("jan", "feb", "mar", "apr", "may", "jum", "jul", "aug", "sep", "oct", "nov", "dec")
      val x = months.indexOf(m.toLowerCase)
      if (x >= 0 && x < months.size) apply(y, x + 1, d)
      else throw ParserException(s"Unable to recognize $m as a month")
  }

  def apply(y: String, m: String, d: String): IsoDate = Try(apply(y.toInt, m, d.toInt)) match {
    case scala.util.Success(x) => x
    case _ => throw ParserException(s"Unable to interpret $y $m $d as a date") // CHECK
  }

  def apply(w: String): IsoDate = {
    val dateR = """([^\-]+)-([^\-]+)-([^\-]+)""".r
    w match {
      case dateR(y, m, d) => apply(if (y.length == 4) y else "20" + y, m, d)
      case _ => throw ParserException(s"Unable to parse $w as a date") // CHECK
    }
  }
}

/**
  * ParserException
  *
  * @param s the message
  */
case class ParserException(s: String) extends Exception(s)

/**
  * FunctionParser
  */
object FunctionParser extends AbstractFunctionParser {
  /**
    * case-independent regular expression to match LIKE
    */
  val rLike: Regex = """(?i)LIKE""".r

  /**
    * case-independent regular expression to match IN
    */
  val rIn: Regex = """(?i)IN""".r

  /**
    * case-independent regular expression to match NOT
    */
  val rNot: Regex = """(?i)NOT""".r

  /**
    * case-independent regular expression to match IS NULL
    */
  val rIsNull: Regex = """(?i)IS NULL""".r

  /**
    * case-independent regular expression to match CONCAT
    */
  val rConcat: Regex = """(?i)CONCAT""".r

}
