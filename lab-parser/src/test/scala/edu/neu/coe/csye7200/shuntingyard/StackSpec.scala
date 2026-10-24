package edu.neu.coe.csye7200.shuntingyard

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class StackSpec extends AnyFlatSpec with Matchers {

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
  it should "iterator0" in {
    val target: Stack[Int] = Stack()
    val xs = target.iterator
    xs.hasNext shouldBe false
  }
  it should "iterator1" in {
    val target: Stack[Int] = Stack(1)
    val xs = target.iterator
    xs.hasNext shouldBe true
    xs.next() shouldBe 1
    xs.hasNext shouldBe false
    target should have size 1
  }
  it should "iterator10" in {
    val target: Stack[Int] = Stack(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val xs = target.iterator
    xs.hasNext shouldBe true
    xs.next() shouldBe 1
    xs.hasNext shouldBe true
    xs.next() shouldBe 2
    xs.hasNext shouldBe true
    xs.next() shouldBe 3
    xs.hasNext shouldBe true
    xs.next() shouldBe 4
    xs.hasNext shouldBe true
    xs.next() shouldBe 5
    xs.hasNext shouldBe true
    xs.next() shouldBe 6
    xs.hasNext shouldBe true
    xs.next() shouldBe 7
    xs.hasNext shouldBe true
    xs.next() shouldBe 8
    xs.hasNext shouldBe true
    xs.next() shouldBe 9
    xs.hasNext shouldBe true
    xs.next() shouldBe 10
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
    target should have size 3
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
