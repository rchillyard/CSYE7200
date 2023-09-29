package edu.neu.coe.csye7200.shuntingyard

import scala.language.implicitConversions

/**
  * Trait to define the behavior of a Stack.
  *
  * @tparam X the underlying type of the stack
  */
trait Stack[+X] extends Iterable[X] {

  self =>

  /**
    * Method to push the value x onto the stack and return a new stack.
    *
    * @param y the value to be pushed.
    * @tparam Y the type of y: a super-type of X.
    * @return a stack which contains the values of this underneath y.
    */
  def push[Y >: X](y: Y): Stack[Y]

  /**
    * Method to pop a value from a stack.
    *
    * @return a tuple of the new stack (without its top value) and the value that was the top, wrapped in Option.
    */
  def pop: (Stack[X], Option[X])

  /**
    * Method to create an Iterator from this Stack.
    * Should only be used to "reverse" a collection.
    *
    * @return an Iterator[X].
    */
  def iterator: Iterator[X] = new Iterator[X] {
    private var stack = self

    def hasNext: Boolean = stack.nonEmpty

    def next(): X = {
      val (z, xo) = stack.pop
      stack = z
      xo.get
    }
  }
}

/**
  * Case class which defines a concrete Stack based on a List.
  *
  * @param xs a list of elements of type X.
  * @tparam X the underlying type of the stack
  */
case class ListStack[+X](xs: List[X]) extends Stack[X] {

  require(xs.nonEmpty)

  def push[Y >: X](y: Y): Stack[Y] = ListStack(y :: xs)

  def pop: (Stack[X], Option[X]) = xs match {
    case Nil => EmptyStack -> None
    case x :: tail => Stack(tail: _*) -> Some(x)
  }

  override def isEmpty: Boolean = xs.isEmpty
}

/**
 * An empty stack.
 */
case object EmptyStack extends Stack[Nothing] {
  def push[Y >: Nothing](y: Y): Stack[Y] = ListStack(y :: Nil)

  def pop: (Stack[Nothing], Option[Nothing]) = (this, None)

  override def isEmpty: Boolean = true
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
  def apply[X]: Stack[X] = EmptyStack

  /**
   * Create a new Stack of type X pre-populated with some number of elements≥
   *
   * @param xs a variable number of X elements.
   * @tparam X the underlying type of the result.
   * @return an empty Stack[X].
   */
  def apply[X](xs: X*): Stack[X] = if (xs.isEmpty) EmptyStack else new ListStack[X](xs.toList)
}