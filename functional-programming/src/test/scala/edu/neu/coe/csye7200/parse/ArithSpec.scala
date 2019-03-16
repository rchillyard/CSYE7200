package edu.neu.coe.csye7200.parse

import org.scalatest.{FlatSpec, Matchers}

/**
  * @author scalaprof
  */
class ArithSpec extends FlatSpec with Matchers {
  behavior of "Arith.factor"
  val fp1 = "1"
  val fp2 = "2"
  it should "be parse 1" in {
    val p = new Arith
    val r = p.parseAll(p.factor, "1")
    r should matchPattern { case p.Success(_, _) => }
    r.get shouldBe fp1
  }
  it should "parse (1+1)" in {
    val p = new Arith
    val r = p.parseAll(p.factor, "(1+1)")
    r should matchPattern { case p.Success(_, _) => }
    val factor1 = p.~(fp1, Nil)
    val plus_term1 = p.~("+", factor1)
    r.get shouldBe p.~(factor1, List(plus_term1))
  }
  behavior of "Arith.term"
  it should "be parse 1" in {
    val p = new Arith
    val term1 =p.~(fp1,Nil)
    val r = p.parseAll(p.term, "1")
    r should matchPattern { case p.Success(_, _) => }
    r.get shouldBe term1
  }
  it should "be parse 1*2" in {
    val p = new Arith
    val r = p.parseAll(p.term, "1*2")
    r should matchPattern { case p.Success(_, _) => }
    r.get shouldBe p.~(fp1, List(p.~("*", fp2)))
  }

  behavior of "Arith.expr"
  it should "be parse 1" in {
    val p = new Arith
    val term1 =p.~(fp1,Nil)
    val expr1 =p.~(term1,Nil)
    val r = p.parseAll(p.expr, "1")
    r should matchPattern { case p.Success(_, _) => }
    r.get shouldBe expr1
  }
  it should "parse 1+1" in {
    val p = new Arith
    val factor1 = p.~(fp1,Nil)
    val plus_term1 =p.~("+",factor1)
    val expr1 =p.~(factor1,List(plus_term1))
    val r = p.parseAll(p.expr, "1+1")
    r should matchPattern { case p.Success(_, _) => }
    r.get shouldBe expr1
  }
  it should "parse 1*2+1" in {
    val p = new Arith
    val r = p.parseAll(p.expr, "1*2+1")
    r should matchPattern { case p.Success(_, _) => }
    val factor1 =p.~(fp1,Nil)
    val times_factor2 =p.~("*",fp2)
    val term1_times_2 =p.~(fp1,List(times_factor2))
    val plus_term1 =p.~("+",factor1)
    val expr1 =p.~(term1_times_2,List(plus_term1))
    r.get shouldBe expr1
  }
}
