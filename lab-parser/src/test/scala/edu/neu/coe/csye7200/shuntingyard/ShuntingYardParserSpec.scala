package edu.neu.coe.csye7200.shuntingyard

import edu.neu.coe.csye7200.CancelOnNotImplemented
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ShuntingYardParserSpec extends AnyFlatSpec with Matchers with CancelOnNotImplemented {

  behavior of "ShuntingYardParser"

  it should "parse parenthesis" in {
    val p = new ShuntingYardParser
    p.parseAll(p.parenthesis, "(").successful shouldBe true
    p.parseAll(p.parenthesis, ")").successful shouldBe true
    p.parseAll(p.parenthesis, "-").successful shouldBe false
  }

  it should "parse operator" in {
    val p = new ShuntingYardParser
    p.parseAll(p.operator, "*").successful shouldBe true
    p.parseAll(p.operator, "+").successful shouldBe true
    p.parseAll(p.operator, "-").successful shouldBe false
  }

  it should "parse value" in {
    val p = new ShuntingYardParser
    p.parseAll(p.value, "1").successful shouldBe true
    p.parseAll(p.value, "-1").successful shouldBe true
    p.parseAll(p.value, "1234567890").successful shouldBe true
    p.parseAll(p.value, "").successful shouldBe false
  }

  it should "parse token" in {
    val p = new ShuntingYardParser
    p.parseAll(p.token, "1").successful shouldBe true
    p.parseAll(p.token, "*").successful shouldBe true
    p.parseAll(p.token, "(").successful shouldBe true
    p.parseAll(p.token, "").successful shouldBe false
  }

  it should "parse infix" in {
    val p = new ShuntingYardParser
    p.parseAll(p.infix, "1").get shouldBe List(Right(Right(1)))
    p.parseAll(p.infix, "-1").get shouldBe List(Right(Right(-1)))
    p.parseAll(p.infix, "(1)").get shouldBe List(Left(Open), Right(Right(1)), Left(Close))
    p.parseAll(p.infix, "(1 + 2)").get shouldBe List(Left(Open), Right(Right(1)), Right(Left(Plus)), Right(Right(2)), Left(Close))
    p.parseAll(p.infix, "(1 + -2)").get shouldBe List(Left(Open), Right(Right(1)), Right(Left(Plus)), Right(Right(-2)), Left(Close))
    p.parseAll(p.infix, "").get shouldBe List()
  }

  it should "parseTokens" in {
    val p = new ShuntingYardParser
    p.parseTokens("1") shouldBe List(Right(Right(1)))
    p.parseTokens("(1)") shouldBe List(Left(Open), Right(Right(1)), Left(Close))
    p.parseTokens("(1 + 2)") shouldBe List(Left(Open), Right(Right(1)), Right(Left(Plus)), Right(Right(2)), Left(Close))
    p.parseTokens("") shouldBe List()
  }

}