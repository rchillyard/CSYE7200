package edu.neu.coe.csye7200.fp.parse

import scala.util.parsing.combinator._

/**
  * @author scalaprof
  */
object ArithImproved extends JavaTokenParsers {

  trait Expression {
    def eval: Double
  }

  abstract class Factor extends Expression

  case class Expr(t: Term, ts: List[String ~ Term]) extends Expression {
    def term(t: String ~ Term): Double = t match {
      case "+" ~ x => x.eval;
      case "-" ~ x => -x.eval
      case x ~ _ => throw ParseException(s"Expr: $x unsupported")
    }

    def eval: Double = ts.foldLeft(t.eval)(_ + term(_))
  }

  case class Term(f: Factor, fs: List[String ~ Factor]) extends Expression {
    def factor(t: String ~ Factor): Double = t match {
      case "*" ~ x => x.eval;
      case "/" ~ x => 1 / x.eval
      case x ~ _ => throw ParseException(s"Term: $x unsupported")
    }

    def eval: Double = fs.foldLeft(f.eval)(_ * factor(_))
  }

  case class FloatingPoint(x: Any) extends Factor {
    def eval: Double = x match {
      case x: String => x.toDouble
      case _ => throw new RuntimeException("FloatingPoint: logic error: x is not a String")
    }
  }

  case class Parentheses(e: Expr) extends Factor {
    def eval: Double = e.eval
  }

  def expr: Parser[Expr] = term ~ rep("+" ~ term | "-" ~ term | failure("expr")) ^^ { case t ~ r => r match {
    case x: List[String ~ Term] => Expr(t, x)
  }
  }

  def term: Parser[Term] = factor ~ rep("*" ~ factor | "/" ~ factor | failure("term")) ^^ { case f ~ r => r match {
    case x: List[String ~ Factor] => Term(f, x)
  }
  }

  def factor: Parser[Factor] = (floatingPointNumber | "(" ~ expr ~ ")" | failure("factor")) ^^ { case "(" ~ e ~ ")" => e match {
    case x: Expr => Parentheses(x)
  };
  case s => FloatingPoint(s)
  }
}

case class ParseException(s: String) extends Exception(s"Parser exception: $s")
