package edu.neu.coe.csye7200.fp.parse

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * @author scalaprof
  */
class ArithImprovedSpec extends AnyFlatSpec with Matchers {
  private val parser = ArithImproved

  behavior of "factor"
  it should "parse 1" in {
    val r = parser.parseAll(parser.factor, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get shouldBe parser.FloatingPoint("1")
  }
  it should "parse (1)" in {
    val r = parser.parseAll(parser.factor, "(1)")
    r should matchPattern { case parser.Success(_, _) => }
    r.get shouldBe parser.Parentheses(parser.Expr(parser.Term(parser.FloatingPoint("1"), Nil), Nil))
  }
  it should "parse pi" in {
    val r = parser.parseAll(parser.factor, "pi")
    r should matchPattern { case parser.Failure("factor", _) => }
  }

  behavior of "term"
  it should "parse 1" in {
    val r = parser.parseAll(parser.term, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get shouldBe parser.Term(parser.FloatingPoint("1"), Nil)
  }
  it should "parse 1*2" in {
    val r = parser.parseAll(parser.term, "1*2")
    r should matchPattern { case parser.Success(_, _) => }
    r.get shouldBe parser.Term(parser.FloatingPoint("1"), List(parser.~("*", parser.FloatingPoint("2"))))
  }
  it should "parse pi" in {
    val r = parser.parseAll(parser.term, "pi")
    r should matchPattern { case parser.Failure("factor", _) => }
  }

  behavior of "expr"
  it should "parse be 1" in {
    val r = parser.parseAll(parser.expr, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.eval shouldBe 1.0
  }
  it should "evaluate 1 as 1.0" in {
    val r = parser.parseAll(parser.expr, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.eval shouldBe 1.0
  }
  it should "evaluate (1+1) as 2.0" in {
    val r = parser.parseAll(parser.expr, "1+1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.eval shouldBe 2.0
  }
  it should "evaluate (1*2+1) as 3.0" in {
    val r = parser.parseAll(parser.expr, "1*2+1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.eval shouldBe 3.0
  }
  it should "evaluate (1*2+1-1.5) as 1.5" in {
    val r = parser.parseAll(parser.expr, "1*2+1-1.5")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.eval shouldBe 1.5
  }
  it should "evaluate (1*2+1-3/2) as 1.5" in {
    val r = parser.parseAll(parser.expr, "1*2+1-3/2")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.eval shouldBe 1.5
  }
  it should "fail to parse (1*2+1-pi/2)" in {
    val r = parser.parseAll(parser.expr, "1*2+1-pi/2")
    r should matchPattern { case parser.Failure(_, _) => }
  }
  it should "fail to parse (1?2)" in {
    val r: parser.ParseResult[parser.Expr] = parser.parseAll(parser.expr, "(1?2)")
    r should matchPattern { case parser.Failure(_, _) => }
    r match {
      case parser.Failure(m, _) => m shouldBe "')' expected but '?' found"
      case _ => fail()
    }
  }
  it should "fail to parse (" in {
    val r = parser.parseAll(parser.expr, "(")
    r should matchPattern { case parser.Failure("factor", _) => }
  }
  it should "fail to parse 1+2=2" in {
    val r = parser.parseAll(parser.expr, "1+2=2")
    r should matchPattern { case parser.Failure(_, _) => }
  }
}
