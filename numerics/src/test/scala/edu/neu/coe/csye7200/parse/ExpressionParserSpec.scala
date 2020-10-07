package edu.neu.coe.csye7200.parse

import edu.neu.coe.csye7200.numerics.Rational
import org.scalatest.flatspec
import org.scalatest.matchers.should

import scala.util._


/**
  * @author scalaprof
  */
class ExpressionParserSpec extends flatspec.AnyFlatSpec with should.Matchers {

  behavior of "DoubleExpressionParser.factor"
  it should "parse 3.1415927" in {
    val parser = DoubleExpressionParser
    parser.parseAll(parser.factor, "3.1415927") should matchPattern { case parser.Success(_, _) => }
  }

  "DoubleExpressionParser(1)" should "be 1.0" in {
    val parser = DoubleExpressionParser
    parser.parseAll(parser.factor, "3.1415927") should matchPattern { case parser.Success(_, _) => }
  }
  it should "parse (1)" in {
    val parser = DoubleExpressionParser
    parser.parseAll(parser.factor, "(1)") should matchPattern { case parser.Success(_, _) => }
  }
  it should "fail pi" in {
    val parser = DoubleExpressionParser
    parser.parseAll(parser.factor, "pi") should matchPattern { case parser.Failure("factor", _) => }
  }

  behavior of "DoubleExpressionParser.term"
  it should "parse 1*2" in {
    val parser = DoubleExpressionParser
    parser.parseAll(parser.term, "1*2") should matchPattern { case parser.Success(_, _) => }
  }
  it should "parse 1/2" in {
    val parser = DoubleExpressionParser
    parser.parseAll(parser.term, "1/2") should matchPattern { case parser.Success(_, _) => }
  }
  it should "parse 3.1415927" in {
    val parser = DoubleExpressionParser
    parser.parseAll(parser.term, "3.1415927") should matchPattern { case parser.Success(_, _) => }
  }
  it should "parse (1)" in {
    val parser = DoubleExpressionParser
    parser.parseAll(parser.term, "(1)") should matchPattern { case parser.Success(_, _) => }
  }
  it should "fail pi" in {
    val parser = DoubleExpressionParser
    parser.parseAll(parser.term, "pi") should matchPattern { case parser.Failure("factor", _) => }
  }
  it should "fail 1+2" in {
    val parser = DoubleExpressionParser
    parser.parseAll(parser.term, "1+2") should matchPattern { case parser.Failure(_, _) => }
  }

  behavior of "DoubleExpressionParser.expr"
  it should "parse 1 as 1.0" in {
    val parser = DoubleExpressionParser
    val r = parser.parseAll(parser.expr, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(1.0) => }
  }
  it should "parse 1+1 as 2.0" in {
    val parser = DoubleExpressionParser
    val r = parser.parseAll(parser.expr, "1+1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(2.0) => }
  }
  it should "parse 1*2+1 as 3.0" in {
    val parser = DoubleExpressionParser
    val r = parser.parseAll(parser.expr, "1*2+1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(3.0) => }
  }
  it should "parse 1*2+1-1.5 as 1.5" in {
    val parser = DoubleExpressionParser
    val r = parser.parseAll(parser.expr, "1*2+1-1.5")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(1.5) => }
  }
  it should "parse 1/0 as infinite" in {
    val parser = DoubleExpressionParser
    val r = parser.parseAll(parser.expr, "1/0")
    r.get.value should matchPattern { case Success(Double.PositiveInfinity) => }
  }
  it should "parse 1*2+1-3/2 as 1.5" in {
    val parser = DoubleExpressionParser
    val r = parser.parseAll(parser.expr, "1*2+1-3/2")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(1.5) => }
  }
  ignore should "fail DoubleExpressionParser(1*2+1-pi/2)" in {
    val parser = DoubleExpressionParser
    val r = parser.parseAll(parser.expr, "2*(2+1-3/2)/3")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(1.0) => }
  }
  it should "fail to parse 1*2+1-pi/2" in {
    val parser = DoubleExpressionParser
    val r = parser.parseAll(parser.expr, "1*2+1-pi/2")
    r should matchPattern { case parser.Failure(_, _) => }
  }
  ignore should "fail to parse (1?2)" in {
    val parser = DoubleExpressionParser
    val r = parser.parseAll(parser.expr, "(1?2)")
    r should matchPattern { case parser.Failure("`)' expected but `?' found", _) => }
  }
  "RationalExpressionParser(1)" should "be 1" in {
    val parser = RationalExpressionParser
    val r = parser.parseAll(parser.expr, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(Rational.one) => }
  }
  "RationalExpressionParser(1+1)" should "be 2/1" in {
    val parser = RationalExpressionParser
    val r = parser.parseAll(parser.expr, "1+1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(Rational(x, Rational.bigOne)) if x.toInt == 2 => }
  }
  it should "parse 1*2+1 as 3/1" in {
    val parser = RationalExpressionParser
    val r = parser.parseAll(parser.expr, "1*2+1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(Rational(x, Rational.bigOne)) if x.toInt == 3 => }
  }
  it should "parse 1*2+1-3/2 as 3/2" in {
    val parser = RationalExpressionParser
    val r = parser.parseAll(parser.expr, "1*2+1-3/2")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(Rational(x, y)) if x.toInt == 3 && y.toInt == 2 => }
  }
  "RationalExpressionParser(1/0)" should "be infinite" in {
    val parser = RationalExpressionParser
    val r = parser.parseAll(parser.expr, "1/0")
    r.get.value should matchPattern { case Success(Rational.infinity) => }
  }
  "(" should "fail" in {
    val parser = DoubleExpressionParser
    val r = parser.parseAll(parser.expr, "(")
    r should matchPattern { case parser.Failure("factor", _) => }
  }
  ignore should "fail 1+2=2" in {
    val parser = DoubleExpressionParser
    val r = parser.parseAll(parser.expr, "1+2=2")
    r should matchPattern { case parser.Failure("expr", _) => }
  }
  "IntExpressionParser(3/2)" should "fail" in {
    val parser = IntExpressionParser
    val r = parser.parseAll(parser.expr, "3/2")
    an[IllegalArgumentException] should be thrownBy r.get.value
  }
}
