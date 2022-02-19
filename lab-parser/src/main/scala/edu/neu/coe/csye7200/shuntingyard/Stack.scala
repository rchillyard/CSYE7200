package edu.neu.coe.csye7200.shuntingyard

import scala.language.implicitConversions

/**
  * Trait to define the behavior of a Stack.
  *
  * @tparam X the underlying type of the stack
  */
trait Stack[X] {

  /**
    * Method to push the value x onto the stack and return a new stack.
    *
    * @param x the value to be pushed.
    * @return a stack which contains the values of this under x.
    */
  def push(x: X): Stack[X]

  /**
    * Method to pop a value from a stack.
    *
    * @return a tuple of the new stack (without its top value) and the value that was the top, wrapped in Option.
    */
  def pop: (Stack[X], Option[X])
}

/**
  * Case class which defines a concrete Stack based on a List.
  *
  * @param xs a list of elements of type X.
  * @tparam X the underlying type of the stack
  */
case class ListStack[X](xs: List[X]) extends Stack[X] {

  def push(x: X): Stack[X] = ListStack(x :: xs)

  def pop: (Stack[X], Option[X]) = xs match {
    case Nil => this -> None
    case x :: tail => ListStack(tail) -> Some(x)
  }
}

/**
  * An empty stack which does not support push or pop as expected.
  */
case object EmptyStack extends Stack[Nothing] {
  def push(x: Nothing): Stack[Nothing] = this

  def pop: (Stack[Nothing], Option[Nothing]) = (this, None)
}

/**
  * Companion object of Stack.
  */
object Stack {
  /**
    * Create a new empty Stack of type X.
    *
    * @tparam X the underlying type of the result.
    * @return an empty Stack[X].
    */
  def apply[X]: Stack[X] = new ListStack[X](Nil)

  /**
    * Create a new Stack of type X pre-populated with some number of elements≥
    *
    * @param xs a variable number of X elements.
    * @tparam X the underlying type of the result.
    * @return an empty Stack[X].
    */
  def apply[X](xs: X*): Stack[X] = new ListStack[X](xs.toList)
}
