package edu.neu.coe.csye7200.fp.parse

import edu.neu.coe.csye7200.fp.parse.Arith.{expr, factor, parseAll, term}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * @author scalaprof
  */
class ArithSpec extends AnyFlatSpec with Matchers {
  behavior of "Arith.factor"
  val fp1 = "1"
  val fp2 = "2"
  it should "be parse 1" in {
    val r = parseAll(Arith.factor, "1")
    r should matchPattern { case Arith.Success(_, _) => }
    r.get shouldBe fp1
  }
  it should "parse (1+1)" in {
    val r = parseAll(factor, "(1+1)")
    r should matchPattern { case Arith.Success(_, _) => }
    val factor1 = Arith.~(fp1, Nil)
    val plus_term1 = Arith.~("+", factor1)
    r.get shouldBe Arith.~(factor1, List(plus_term1))
  }
  behavior of "Arith.term"
  it should "be parse 1" in {
    val term1 = Arith.~(fp1, Nil)
    val r = parseAll(term, "1")
    r should matchPattern { case Arith.Success(_, _) => }
    r.get shouldBe term1
  }
  it should "be parse 1*2" in {
    val r = Arith.parseAll(term, "1*2")
    r should matchPattern { case Arith.Success(_, _) => }
    r.get shouldBe Arith.~(fp1, List(Arith.~("*", fp2)))
  }

  behavior of "Arith.expr"
  it should "be parse 1" in {
    val term1 = Arith.~(fp1, Nil)
    val expr1 = Arith.~(term1, Nil)
    val r = Arith.parseAll(expr, "1")
    r should matchPattern { case Arith.Success(_, _) => }
    r.get shouldBe expr1
  }
  it should "parse 1+1" in {
    val factor1 = Arith.~(fp1, Nil)
    val plus_term1 = Arith.~("+", factor1)
    val expr1 = Arith.~(factor1, List(plus_term1))
    val r = Arith.parseAll(expr, "1+1")
    r should matchPattern { case Arith.Success(_, _) => }
    r.get shouldBe expr1
  }
  it should "parse 1*2+1" in {
    val r = Arith.parseAll(expr, "1*2+1")
    r should matchPattern { case Arith.Success(_, _) => }
    val factor1 = Arith.~(fp1, Nil)
    val times_factor2 = Arith.~("*", fp2)
    val term1_times_2 = Arith.~(fp1, List(times_factor2))
    val plus_term1 = Arith.~("+", factor1)
    val expr1 = Arith.~(term1_times_2, List(plus_term1))
    r.get shouldBe expr1
  }
}
