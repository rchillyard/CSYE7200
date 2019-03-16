package edu.neu.coe.csye7200.parse

import org.scalatest.{FlatSpec, Matchers}

/**
  * @author scalaprof
  */
class ArithImprovedSpec extends FlatSpec with Matchers {
  "1" should "be 1.0" in {
    val parser = new ArithImproved
    val r = parser.parseAll(parser.expr, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.eval shouldBe 1.0
  }
  "(1)" should "be 1.0" in {
    val parser = new ArithImproved
    val r = parser.parseAll(parser.expr, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.eval shouldBe 1.0
  }
  "(1+1)" should "be 2.0" in {
    val parser = new ArithImproved
    val r = parser.parseAll(parser.expr, "1+1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.eval shouldBe 2.0
  }
  "(1*2+1)" should "be 3.0" in {
    val parser = new ArithImproved
    val r = parser.parseAll(parser.expr, "1*2+1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.eval shouldBe 3.0
  }
  "(1*2+1-1.5)" should "be 1.5" in {
    val parser = new ArithImproved
    val r = parser.parseAll(parser.expr, "1*2+1-1.5")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.eval shouldBe 1.5
  }
  "(1*2+1-3/2)" should "be 1.5" in {
    val parser = new ArithImproved
    val r = parser.parseAll(parser.expr, "1*2+1-3/2")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.eval shouldBe 1.5
  }
  "(1*2+1-pi/2)" should "fail" in {
    val parser = new ArithImproved
    val r = parser.parseAll(parser.expr, "1*2+1-pi/2")
    r should matchPattern { case parser.Failure("factor", _) => }
  }
  "(1?2)" should "fail" in {
    val parser = new ArithImproved
    val r = parser.parseAll(parser.expr, "(1?2)")
    r should matchPattern { case parser.Failure("`)' expected but `?' found", _) => }
  }
  "(" should "fail" in {
    val parser = new ArithImproved
    val r = parser.parseAll(parser.expr, "(")
    r should matchPattern { case parser.Failure("factor", _) => }
  }
  "1+2=2" should "fail" in {
    val parser = new ArithImproved
    val r = parser.parseAll(parser.expr, "1+2=2")
    r should matchPattern { case parser.Failure("expr", _) => }
  }
}
