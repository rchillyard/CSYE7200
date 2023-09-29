package edu.neu.coe.csye7200.shuntingyard

import org.scalatest.{FlatSpec, Matchers}

class StackSpec extends FlatSpec with Matchers {

  behavior of "Stack.apply"

  it should "apply" in {
    Stack() shouldBe EmptyStack
  }
  it should "push" in {
    val target: Stack[Int] = Stack()
    val xs = target.push(1)
    xs.nonEmpty shouldBe true
    xs shouldBe ListStack(List(1))
  }
  it should "pop" in {
    val target: Stack[Int] = Stack(1)
    val (xs, xo) = target.pop
    xs shouldBe EmptyStack
    xs.isEmpty shouldBe true
    xo shouldBe Some(1)
  }
  it should "iterator1" in {
    val target: Stack[Int] = Stack(1)
    val xs = target.iterator
    xs.hasNext shouldBe true
    xs.next() shouldBe 1
    xs.hasNext shouldBe false
    target.size shouldBe 1
  }
  it should "iterator2" in {
    val target: Stack[Int] = Stack()
    val xs = target.iterator
    xs.hasNext shouldBe false
  }
  it should "isEmpty1" in {
    val target: Stack[Int] = Stack(1)
    target.isEmpty shouldBe false
  }
  it should "isEmpty2" in {
    val target: Stack[Int] = Stack()
    target.isEmpty shouldBe true
  }

  behavior of "ListStack"

  it should "construct a ListStack" in {
    val target = new ListStack[Int](List(1, 2, 3))
    target.nonEmpty shouldBe true
    target.size shouldBe 3
  }

  it should "throw exception for new ListStack(Nil)" in {
    an[IllegalArgumentException] should be thrownBy new ListStack[Nothing](Nil)
  }

  behavior of "EmptyStack"

  it should "push" in {
    val target = EmptyStack
    target.push(1) shouldBe ListStack(List(1))
  }
  it should "pop" in {
    val target = EmptyStack
    target.pop shouldBe(EmptyStack, None)
  }
  it should "iterator" in {
    val target = EmptyStack
    val xs = target.iterator
    xs.hasNext shouldBe false
  }
  it should "isEmpty" in {
    val target = EmptyStack
    target.isEmpty shouldBe true
  }

}