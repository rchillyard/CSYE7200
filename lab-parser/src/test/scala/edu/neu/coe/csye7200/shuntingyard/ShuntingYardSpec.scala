package edu.neu.coe.csye7200.shuntingyard

import edu.neu.coe.csye7200.CancelOnNotImplemented
import edu.neu.coe.csye7200.parse.Token
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class ShuntingYardSpec extends AnyFlatSpec with Matchers with CancelOnNotImplemented {

  behavior of "ShuntingYard"

  it should "empty" in {
    val target: ShuntingYard = ShuntingYard.empty
    target shouldBe ShuntingYard(EmptyStack, EmptyStack, 0)
  }

  behavior of "evaluate"

  it should "evaluate1" in {
    ShuntingYard.evaluate("(1 + 2)") shouldBe Some(3)
  }

  it should "evaluate2" in {
    ShuntingYard.evaluate("(1 + ((2+3) * (4*5)))") shouldBe Some(101)
  }

  it should "evaluate3" in {
    ShuntingYard.evaluate("1 + ((2+3) * (4*5))") shouldBe Some(101)
  }

  it should "evaluate4" in {
    ShuntingYard.evaluate("1 + (2+3) * 4*5") shouldBe Some(101)
  }

  it should "evaluate5" in {
    ShuntingYard.evaluate("(1 + (2+3) * 4*5") shouldBe None
    ShuntingYard.evaluate("1 + (2+3) * 4*5)") shouldBe None
  }

  behavior of "apply"

  it should "apply" in {
    val target = ShuntingYard(Stack(1), Stack.empty, 0)
    target.apply shouldBe Some(1)
  }

  private val open: Token = Left(Open)

  it should "apply(Token) 1" in {
    val target: ShuntingYard = ShuntingYard.empty
    target.apply(open) shouldBe target.copy(depth = 1)
  }

  it should "apply(Token) 2" in {
    val target: ShuntingYard = ShuntingYard.empty
    val expected = ShuntingYard(Stack(1), Stack.empty, 0)
    target(Right(Right(1))) shouldBe expected
  }

  it should "apply(Token) 3" in {
    val target: ShuntingYard = ShuntingYard.empty(open)
    val expected1 = ShuntingYard(Stack(1), Stack.empty, 1)
    val expected2 = ShuntingYard(Stack(1), Stack(Plus), 1)
    val expected3 = ShuntingYard(Stack(2, 1), Stack(Plus), 1)
    val result1 = target(Right(Right(1)))
    result1 shouldBe expected1
    val result2 = result1(Right(Left(Operator("+"))))
    result2 shouldBe expected2
    val result3 = result2(Right(Right(2)))
    result3 shouldBe expected3
    val result4 = result3(Left(Close))
    result4 shouldBe ShuntingYard(Stack(3), Stack.empty[Operator], 0)
    result4.apply shouldBe Some(3)
  }

  it should "apply 4" in {
    val target: ShuntingYard = ShuntingYard.empty
    val expected1 = ShuntingYard(Stack(1), Stack.empty[Operator], 0)
    val expected2 = ShuntingYard(Stack(1), Stack(Plus), 0)
    val expected3 = ShuntingYard(Stack(2, 1), Stack(Plus), 0)
    val result1 = target(Right(Right(1)))
    result1 shouldBe expected1
    val result2 = result1(Right(Left(Operator("+"))))
    result2 shouldBe expected2
    val result3 = result2(Right(Right(2)))
    result3 shouldBe expected3
    result3.apply shouldBe Some(3)
  }

}
