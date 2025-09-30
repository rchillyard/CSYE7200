package edu.neu.coe.csye7200.shuntingyard

import edu.neu.coe.csye7200.parse.ParserException
import scala.util.parsing.combinator.JavaTokenParsers

/**
 * A parser implementation based on the Shunting Yard algorithm designed to parse mathematical infix expressions
 * into a list of tokens that can represent parentheses, operators, or integers.
 * The class extends `JavaTokenParsers` to leverage its base functionality for parsing.
 *
 * You find JavaTokenParsers in "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
 */
class ShuntingYardParser extends JavaTokenParsers {

  /**
   * A type alias representing a token used in the Shunting Yard algorithm.
   * A token can be one of three possible types:
   * - `Parenthesis` represented by `Left(Parenthesis)`
   * - `Operator` represented by `Right(Left(Operator))`
   * - `Int` values represented by `Right(Right(Int))`
   */
  type Token = Either[Parenthesis, Either[Operator, Int]]

  /**
   * Parses a given infix mathematical expression string and transforms it into a list of tokens.
   * Tokens can represent parentheses, operators, or integers, which are defined by the type alias `Token`.
   * The parsing process uses the Shunting Yard algorithm to tokenize the input string.
   *
   * @param w the input string containing an infix mathematical expression to be parsed
   * @return a list of tokens representing the parsed input expression
   * @throws ParserException if the parsing process fails due to an invalid input format
   */
  def parseTokens(w: String): List[Token] =
    parseAll(infix, w) match {
      case Success(xs, _) => xs
      case _ => throw ParserException("parse error")
    }

  /**
   * Parses a sequence of tokens representing an infix mathematical expression.
   * The tokens are expected to follow the defined grammar for parentheses, operators, and integer values.
   *
   * @return a parser that produces a list of tokens representing the parsed expression.
   */
  def infix: Parser[List[Token]] =
    rep(token)

  /**
   * Parses a single token from the input string. A token can represent a parenthesis,
   * a value (integer), or an operator. The method attempts to match the input
   * against any of the defined parsers (`parenthesis`, `value`, or `operator`).
   *
   * @return a `Parser` that parses and returns a `Token` representing either a parenthesis,
   *         an operator, or an integer value.
   */
  def token: Parser[Token] =
    parenthesis | value | operator

  /**
   * Parses a single parenthesis character ("(" or ")") from the input string
   * and wraps it into a `Parenthesis` token.
   *
   * @return a parser that parses a parenthesis character and produces a `Token`
   *         in the form of `Left(Parenthesis)`.
   */
  def parenthesis: Parser[Token] =
    parens ^^ { x => Left(Parenthesis(x)) }

  /**
   * Parses a numeric value from the input, represented as a token of type `Int`.
   *
   * This method matches a sequence of digits using a regular expression, converts the matched string into an integer,
   * and wraps it in the token type `Right(Right(Int))`.
   *
   * @return a parser that processes numeric strings and outputs a token representing an integer value.
   */
  def value: Parser[Token] =
    digits ^^ { x => Right(Right(x.toInt)) }

  /**
   * Parses an operator token from the input string. The operator token can be either `*` or `+`.
   * If the input matches one of these operators, it is wrapped in the `Operator` type and subsequently
   * returned as part of the `Token` type.
   *
   * @return a `Parser` that recognizes operator tokens (`*` or `+`) and produces a `Token` of type
   *         `Right(Left(Operator))` upon a successful match.
   */
  def operator: Parser[Token] =
    operators ^^ { x => Right(Left(Operator(x))) }

  /**
   * A parser that matches a single left or right parenthesis character, `(` or `)`.
   *
   * @return a `Parser[String]` that recognizes and parses either `(` or `)` as a string.
   */
  private def parens: Parser[String] = """[()]""".r

  /**
   * A parser that matches one or more numeric digits (0-9) in the input string.
   * This parser uses a regular expression to identify sequences of digits.
   *
   * @return a `Parser[String]` that recognizes and parses numeric strings consisting of one or more digits.
   */
  private def digits: Parser[String] = """\d+""".r

  /**
   * A parser that matches operator characters `*` or `+`.
   * The matched string represents the operator.
   *
   * @return a `Parser[String]` that recognizes and parses the `*` or `+` operators.
   */
  private def operators: Parser[String] = """[*+]""".r
}