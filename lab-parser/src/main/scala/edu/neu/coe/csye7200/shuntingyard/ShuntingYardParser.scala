package edu.neu.coe.csye7200.shuntingyard

import edu.neu.coe.csye7200.parse.ParserException

import scala.util.parsing.combinator.JavaTokenParsers

class ShuntingYardParser extends JavaTokenParsers {

  type Token = Either[Parenthesis, Either[Operator, Int]]

  def parseTokens(w: String): List[Token] = parseAll(infix, w) match {
    case Success(xs, _) => xs
    case _ => throw ParserException("parse error")
  }

  def infix: Parser[List[Token]] = rep(token)

  def token: Parser[Token] = parenthesis | value | operator

  def parenthesis: Parser[Token] = """[()]""".r ^^ { x => Left(Parenthesis(x)) }

  def value: Parser[Token] = """\d+""".r ^^ { x => Right(Right(x.toInt)) }

  def operator: Parser[Token] = """[*+]""".r ^^ { x => Right(Left(Operator(x))) }

}

case class Parenthesis(open: Boolean)

object Parenthesis {
  def apply(w: String): Parenthesis = w match {
    case "(" => Parenthesis(true)
    case ")" => Parenthesis(false)
  }
}

trait Operator {
  def operate(x: Int, y: Int): Int
}

object Operator {
  def apply(s: String): Operator = s match {
    case "+" => Plus
    case "*" => Times
  }
}

case object Plus extends Operator {
  def operate(x: Int, y: Int): Int = x + y
}

case object Times extends Operator {
  def operate(x: Int, y: Int): Int = x * y
}