package edu.neu.coe.csye7200.shuntingyard

import org.scalatest.{FlatSpec, Matchers}

class ShuntingYardSpec extends FlatSpec with Matchers {

  behavior of "ShuntingYardSpec"

  it should "evaluate1" in {
    ShuntingYard.evaluate("(1 + 2)") shouldBe Some(3)
  }

  it should "evaluate2" in {
    ShuntingYard.evaluate("(1 + ((2+3) * (4*5)))") shouldBe Some(101)
  }

  it should "apply" in {
    val target = ShuntingYard(Stack(1), Stack[Operator])
    target.apply shouldBe Some(1)
  }

  it should "apply(Token) 1" in {
    val target: ShuntingYard = ShuntingYard.apply
    target.apply(Left(Parenthesis(true))) shouldBe target
  }

  it should "apply(Token) 2" in {
    val target: ShuntingYard = ShuntingYard.apply
    val expected = ShuntingYard(Stack(1), Stack[Operator])
    target(Right(Right(1))) shouldBe expected
  }

  it should "apply(Token) 3" in {
    val target: ShuntingYard = ShuntingYard.apply
    val expected1 = ShuntingYard(Stack(1), Stack[Operator])
    val expected2 = ShuntingYard(Stack(1), Stack(Plus))
    val expected3 = ShuntingYard(Stack(2, 1), Stack(Plus))
    val result1 = target(Right(Right(1)))
    result1 shouldBe expected1
    val result2 = result1(Right(Left(Operator("+"))))
    result2 shouldBe expected2
    val result3 = result2(Right(Right(2)))
    result3 shouldBe expected3
    result3(Left(Parenthesis(false))) shouldBe ShuntingYard(Stack(3), Stack[Operator])
  }

}
