package edu.neu.coe.csye7200.parse

import scala.util.parsing.combinator._
import scala.util.Try

/**
 * @author scalaprof
 */
abstract class ExpressionParser[T] extends JavaTokenParsers with (String => Try[T]) { self =>

  def apply(s: String): Try[T]
  def div: (T,T)=>T
  def negate: (T)=>T
  def plus: (T,T)=>T
  def times: (T,T)=>T
  def one: T
  def zero: T
  
  trait Expression {
    def value: Try[T]
  }

  def lift(t: Try[T])(f: (T) => T): Try[T] = t map f
  def map2(t1: Try[T], t2: Try[T])(f: (T,T) => T): Try[T] = for { tt1 <- t1 ; tt2 <- t2 } yield f(tt1,tt2)
  
  abstract class Factor extends Expression
  case class Expr(t: Term, ts: List[String~Term]) extends Expression {
    def termVal(t: String~Term): Try[T] = t match {case "+"~x => x.value; case "-"~x => lift(x.value)(negate); case z~_ => scala.util.Failure(ParseException(s"Expr: operator $z is not supported")) }
    def value: Try[T] = ts.foldLeft(t.value)((a, x) => map2(a,termVal(x))(plus))
  }
  case class Term(f: Factor, fs: List[String~Factor]) extends Expression {
    def factorVal(t: String~Factor): Try[T] = t match {case "*"~x => x.value; case "/"~x => map2(Try(one),x.value)(div); case z~_ => scala.util.Failure(ParseException(s"Term: operator $z is not supported")) }
    def value = fs.foldLeft(f.value)((a,x)=>map2(a,factorVal(x))(times))
  }
  case class FloatingPoint(x: String) extends Factor {
    def value = self.apply(x)
  }
  case class Parentheses(e: Expr) extends Factor {
    def value: Try[T] = e.value
  }
  
  def expr: Parser[Expr] = term~rep("+"~term | "-"~term | failure("expr")) ^^ { case t~r => r match {case x: List[String~Term] => Expr(t,x)}}
  def term: Parser[Term] = factor~rep("*"~factor | "/"~factor | failure("term")) ^^ { case f~r => r match {case x: List[String~Factor] => Term(f,x)}} 
  def factor: Parser[Factor] = (floatingPointNumber | "("~expr~")" | failure("factor")) ^^ { case "("~e~")" => e match {case x: Expr => Parentheses(x)}; case s: String => FloatingPoint(s) }
}

case class ParseException(s: String) extends Exception(s"Parse exception: $s")
