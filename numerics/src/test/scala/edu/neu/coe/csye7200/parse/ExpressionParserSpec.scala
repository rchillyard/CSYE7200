package edu.neu.coe.csye7200.parse

import org.scalatest.{ FlatSpec, Matchers }
import edu.neu.coe.csye7200.numerics.Rational
import scala.util._
import edu.neu.coe.csye7200.numerics.Rational._


/**
 * @author scalaprof
 */
class ExpressionParserSpec extends FlatSpec with Matchers {
  "DoubleExpressionParser(1)" should "be 1.0" in {
    val parser = DoubleExpressionParser
    val r = parser.parseAll(parser.expr, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(1.0) => }
  }
  "DoubleExpressionParser(1+1)" should "be 2.0" in {
    val parser = DoubleExpressionParser
    val r = parser.parseAll(parser.expr, "1+1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(2.0) => }
  }
  "DoubleExpressionParser(1*2+1)" should "be 3.0" in {
    val parser = DoubleExpressionParser
    val r = parser.parseAll(parser.expr, "1*2+1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(3.0) => }
  }
  "DoubleExpressionParser(1*2+1-1.5)" should "be 1.5" in {
    val parser = DoubleExpressionParser
    val r = parser.parseAll(parser.expr, "1*2+1-1.5")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(1.5) => }
  }
  "DoubleExpressionParser(1/0)" should "be infinite" in {
    val parser = DoubleExpressionParser
    val r = parser.parseAll(parser.expr, "1/0")
    r.get.value should matchPattern { case Success(Double.PositiveInfinity) => }
  }
  "DoubleExpressionParser(1*2+1-3/2)" should "be 1.5" in {
    val parser = DoubleExpressionParser
    val r = parser.parseAll(parser.expr, "1*2+1-3/2")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(1.5) => }
  }
  "DoubleExpressionParser(1*2+1-pi/2)" should "fail" in {
    val parser = DoubleExpressionParser
    val r = parser.parseAll(parser.expr, "1*2+1-pi/2")
    r should matchPattern { case parser.Failure("factor", _) => }
  }
  "DoubleExpressionParser(1?2)" should "fail" in {
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
    r.get.value should matchPattern { case Success(Rational(2,1)) => }
  }
  "RationalExpressionParser(1*2+1)" should "be 3/1" in {
    val parser = RationalExpressionParser
    val r = parser.parseAll(parser.expr, "1*2+1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(Rational(3,1)) => }
  }
  "RationalExpressionParser(1*2+1-3/2)" should "be 3/2" in {
    val parser = RationalExpressionParser
    val r = parser.parseAll(parser.expr, "1*2+1-3/2")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(Rational(3,2)) => }
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
  "1+2=2" should "fail" in {
    val parser = DoubleExpressionParser
    val r = parser.parseAll(parser.expr, "1+2=2")
    r should matchPattern { case parser.Failure("expr", _) => }
  }
  "IntExpressionParser(3/2)" should "fail" in {
    val parser = IntExpressionParser
    val r = parser.parseAll(parser.expr, "3/2")
    an [IllegalArgumentException] should be thrownBy r.get.value
  }
}
