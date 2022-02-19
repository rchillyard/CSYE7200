package edu.neu.coe.csye7200.shuntingyard

import org.scalatest.{FlatSpec, Matchers}

class StackSpec extends FlatSpec with Matchers {

  behavior of "Stack"

  it should "push" in {
    val target: Stack[Int] = Stack()
    target.push(1) shouldBe ListStack(List(1))
  }
  it should "pop" in {
    val target: Stack[Int] = Stack(1)
    target.pop shouldBe (Stack(), Some(1))
  }

}
