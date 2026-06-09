package edu.neu.coe.csye7200.fp.parse

import scala.util.Try
import scala.util.parsing.combinator._

/**
 * @author scalaprof
 */
object ArithImproved extends JavaTokenParsers {

  trait Expression {
    def eval: Double
  }

  trait Factor extends Expression

  case class Expr(t: Term, ts: List[String ~ Term]) extends Expression {
    def term(t: String ~ Term): Double = t match {
      case "+" ~ x => x.eval;
      case "-" ~ x => -x.eval
      case x ~ _ => throw ParseException(s"Expr.term: prefix $x unsupported")
    }

    def eval: Double = ts.foldLeft(t.eval)(_ + term(_))
  }

  case class Term(f: Factor, fs: List[String ~ Factor]) extends Expression {
    def factor(t: String ~ Factor): Double = t match {
      case "*" ~ x => x.eval;
      case "/" ~ x => 1 / x.eval
      case x ~ _ => throw ParseException(s"Term.factor: prefix $x unsupported")
    }

    def eval: Double = fs.foldLeft(f.eval)(_ * factor(_))
  }

  case class Coefficient(x: Double) extends Factor {
    def eval: Double = x
  }

  case class InvalidFactor(reason: Any) extends Factor {
    def eval: Double = Double.NaN
  }

  case class Parentheses(e: Expr) extends Factor {
    def eval: Double = e.eval
  }

  def expr: Parser[Expr] = term ~ rep("+" ~ term | "-" ~ term | failure("expr")) ^^ {
    case t ~ r => Expr(t, r)
  }

  def term: Parser[Term] = factor ~ rep("*" ~ factor | "/" ~ factor | failure("term")) ^^ {
    case f ~ r => Term(f, r)
  }

  def factor: Parser[Factor] = (floatingPointNumber | "(" ~> expr <~ ")" | failure("factor")) ^^ {
    case e: Expr => Parentheses(e)
    case s: String => Try(Coefficient(s.toDouble)) getOrElse (InvalidFactor(s"$s is cannot be parsed as a double"))
    case x => InvalidFactor(x)
  }
}

case class ParseException(s: String) extends Exception(s"Parser exception: $s")