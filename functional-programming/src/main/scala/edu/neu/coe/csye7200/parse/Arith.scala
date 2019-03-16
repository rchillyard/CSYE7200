package edu.neu.coe.csye7200.parse

import scala.util.parsing.combinator._

/**
  * @author scalaprof
  */
class Arith extends JavaTokenParsers {

  def expr: Parser[Any] = term ~ rep("+" ~ term | "-" ~ term)

  def term: Parser[Any] = factor ~ rep("*" ~ factor | "/" ~ factor)

  def factor: Parser[Any] = floatingPointNumber | "(" ~> expr <~ ")"
}
