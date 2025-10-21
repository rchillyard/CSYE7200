package edu.neu.coe.csye7200.parse

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * A parser implementation designed to parse mathematical infix expressions
 * into a list of tokens that can represent brackets (parentheses), + and * operators, or integers.
 * The class extends `JavaTokenParsers` to leverage its base functionality for parsing.
 *
 * You find JavaTokenParsers in "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0".
 */
class InfixParser extends JavaTokenParsers {

  type Token = Either[Bracket, Either[AddOrMultiply, Int]]

  /**
   * Parses a given infix mathematical expression string and transforms it into a list of tokens.
   * Tokens can represent parentheses, operators, or integers, which are defined by the type alias `Token`.
   *
   * @param w the input string containing an infix mathematical expression to be parsed
   * @return a list of tokens representing the parsed input expression
   * @throws ParserException if the parsing process fails due to an invalid input format
   */
  def parseTokens(w: String): List[Token] = {
    // TO BE IMPLEMENTED : parse a string of numbers, operators, and round brackets (parentheses) into a list of tokens.
    // These tokens will be separated in the input by white space)
        ???
  }

  /**
   * Parses a sequence of tokens representing an infix mathematical expression.
   * The tokens are expected to follow the defined grammar for brackets (parentheses), operators, and integer values.
   *
   * @return a parser that produces a list of tokens representing the parsed expression.
   */
  def infix: Parser[List[Token]] = {
    // TO BE IMPLEMENTED : parse a sequence of tokens into a list.
    // NOTE that white space is already ignored and so you don't need a delimiter for this repetition)
        ???
  }

  /**
   * Parses a single token from the input string. A token can represent a bracket,
   * a value (integer), or an operator. The method attempts to match the input
   * against any of the defined parsers (`bracket`, `value`, or `operator`).
   *
   * @return a `Parser` that parses and returns a `Token` representing either a bracket,
   *         an operator, or an integer value.
   */
  def token: Parser[Token] =
    bracket | integer | operator

  /**
   * Parses a single bracket character ("(" or ")") from the input string
   * and wraps it into a `Bracket` token.
   *
   * @return a parser that parses a bracket character and produces a `Token`
   *         in the form of `Left(Bracket)`.
   */
  def bracket: Parser[Token] = {
    // TO BE IMPLEMENTED : parse a single bracket character (in the form of a String) and wrap it in a Bracket token.
    // Use the brackets parser below.
        ???
  }

  /**
   * Parses a numeric value from the input, represented as a token of type `Int`.
   *
   * This method matches a sequence of digits using a regular expression, converts the matched string into an integer,
   * and wraps it in the token type `Right(Right(Int))`.
   *
   * @return a parser that processes numeric strings and outputs a token representing an integer value.
   */
  def integer: Parser[Token] = {
    // TO BE IMPLEMENTED : parse a single integer value (in the form of a String) and return it as a Right(Right(Int)) token
        ???
  }

  /**
   * Parses an operator token from the input string. The operator token can be either `*` or `+`.
   * If the input matches one of these operators, it is wrapped in the `AddOrMultiply` type and subsequently
   * returned as part of the `Token` type.
   *
   * @return a `Parser` that recognizes operator tokens (`*` or `+`) and produces a `Token` of type
   *         `Right(Left(AddOrMultiply))` upon a successful match.
   */
  def operator: Parser[Token] = {
    // TO BE IMPLEMENTED : parse an operator token (in the form of a String) and return it as a Right(Left(AddOrMultiply)) token
    // Use the operators parser below.
        ???
  }

  /**
   * A parser that matches a single left or right bracket character, `(` or `)`.
   *
   * @return a `Parser[String]` that recognizes and parses either `(` or `)` as a string.
   */
  private def brackets: Parser[String] = """[()]""".r

  /**
   * A parser that matches operator characters `*` or `+`.
   * The matched string represents the operator.
   *
   * @return a `Parser[String]` that recognizes and parses the `*` or `+` operators.
   */
  private def operators: Parser[String] = """[*+]""".r
}


/**
 * Trait representing a (round) bracket, aka parenthesis, which can either be open or closed.
 */
trait Bracket

/**
 * Represents an open bracket.
 */
case object Open extends Bracket {
  override def toString: String = "("
}

/**
 * Represents a closing bracket.
 */
case object Close extends Bracket {
  override def toString: String = ")"
}

/**
 * Companion object for the Bracket case class.
 * Provides a method to construct a Bracket instance based on a string input.
 */
object Bracket {
  /**
   * Constructs a `Bracket` instance based on the input string.
   * The input is expected to be either "(" or ")" to signify an open or closed bracket respectively.
   *
   * @param w a string representing a bracket, where "(" indicates an open bracket and ")" indicates a closed one.
   * @return a `Bracket` instance with a boolean value `true` for open bracket and `false` for closed bracket.
   * @throws MatchError if the input string is not "(" or ")".
   */
  def apply(w: String): Bracket = w match {
    case "(" => Open
    case ")" => Close
  }
}

/**
 * Trait representing an AddOrMultiply, typically used in mathematical or computational contexts.
 *
 * This trait defines a contract for any class or object that implements it to provide
 * a specific `operate` method, which takes two integer operands and returns an integer result.
 */
trait AddOrMultiply {
  /**
   * Applies a add/multiply operation between two integers and returns the result.
   *
   * @param x the first integer operand.
   * @param y the second integer operand.
   * @return the result of applying the operation to the given operands.
   */
  def operate(x: Int, y: Int): Int
}

/**
 * Companion object for the AddOrMultiply trait.
 *
 * Provides utility methods for creating specific AddOrMultiply instances based on a string.
 */
object AddOrMultiply {
  /**
   * Creates an AddOrMultiply instance based on the provided string representation.
   *
   * This method maps specific string inputs to their corresponding AddOrMultiply case objects.
   *
   * @param s a string representation of the operator ("+" for Plus, "*" for Times).
   * @return the corresponding AddOrMultiply instance (e.g., `Plus` or `Times`) if the input matches a valid operator.
   */
  def apply(s: String): AddOrMultiply = s match {
    case "+" => Plus
    case "*" => Times
  }
}

/**
 * Represents the addition operator in a mathematical operation.
 *
 * This object extends the `AddOrMultiply` trait and specifically implements the `operate`
 * method to perform the addition of two integers.
 *
 * The `Plus` operator is used to compute the sum of two integers.
 */
case object Plus extends AddOrMultiply {
  /**
   * Applies an operation on two integer operands and returns the result.
   *
   * @param x the first integer operand.
   * @param y the second integer operand.
   * @return the result of applying the operation, which in this case is the sum of x and y.
   */
  def operate(x: Int, y: Int): Int = x + y
}

/**
 * A case object representing the multiplication operator.
 *
 * This object extends the `AddOrMultiply` trait and provides
 * an implementation of the `operate` method to multiply two integers.
 */
case object Times extends AddOrMultiply {
  /**
   * Method to perform an operation on two integers and return the result.
   *
   * @param x the first integer operand.
   * @param y the second integer operand.
   * @return the result of multiplying the two integer operands.
   */
  def operate(x: Int, y: Int): Int = x * y
}