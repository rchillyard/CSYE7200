package edu.neu.coe.csye7200.shuntingyard

import edu.neu.coe.csye7200.parse.Token
import scala.language.implicitConversions

/**
 * Case class which supports the Shunting Yard algorithm of Dijkstra.
 *
 * See [[https://en.wikipedia.org/wiki/Shunting_yard_algorithm]]
 *
 * @param valueStack    a Stack[Int].
 * @param operatorStack a Stack[Operator].
 * @param depth         the current depth of nested parentheses.
 */
case class ShuntingYard(valueStack: Stack[Int], operatorStack: Stack[Operator], depth: Int) extends (Token => ShuntingYard) {

  self =>

  /**
    * Method to transform this ShuntingYard according to the token value t.
    *
    * @param t a ShuntingYardParse#Token.
    * @return (usually) a new ShuntingYard.
    */
  def apply(t: Token): ShuntingYard = t match {
    case Right(Left(operator)) =>
      copy(operatorStack = operatorStack.push(operator))
    case Right(Right(value)) =>
      copy(valueStack = valueStack.push(value))
    case Left(Open) =>
      copy(depth = depth + 1) // for now, we ignore left parenthesis
    case Left(Close) =>
      evaluate(depth - 1)
  }

  /**
    * Method to evaluate this ShuntingYard as an optional Int.
    * The depth of this ShuntingYard must be zero, otherwise None will be returned.
    *
    * @return an optional Int value.
    */
  def apply: Option[Int] = self match {
    // XXX Terminating condition: If this ShuntingYard has depth of 0 and exactly one value and no operators,
    //    then we return that value wrapped in Some; otherwise we return None.
    case ShuntingYard(ListStack(x :: Nil), EmptyStack, 0) =>
      Some(x)
    // XXX Recursive case: If this ShuntingYard has at least two values and one operator,
    //    then we recursively call apply to the evaluated version of this.
    //    Normally, d is zero but occasionally it will be non-zero and we need to keep track of it.
    case ShuntingYard(ListStack(_ :: _ :: _), ListStack(_ :: _), d) =>
      evaluate(d).apply
    // Otherwise, we must return None.
    case _ =>
      None
  }

  private def evaluate(d: Int): ShuntingYard = {
    // When we evaluate this ShuntingYard,
    // we pop the top operator and the two top values from their respective stacks,
    // apply the operator and push the resulting value onto the value stack.

    // TO BE IMPLEMENTED 
        ???
  }

  override def toString(): String = s"ShuntingYard($valueStack,$operatorStack,$depth)"
}

/**
  * Companion object to ShuntingYard.
  */
object ShuntingYard {
  /**
   * Constructs a new instance of the ShuntingYard class with an empty value stack,
   * an empty operator stack, and an initial depth of 0.
   *
   * @return a new ShuntingYard instance initialized with default stacks and depth.
   */
  def empty: ShuntingYard = new ShuntingYard(Stack.empty[Int], Stack.empty[Operator], 0)

  /**
   * Converts an instance of `ShuntingYard` to an `Option[Int]` by evaluating the given ShuntingYard instance.
   *
   * @param s the ShuntingYard instance to be evaluated.
   * @return an optional integer value resulting from the evaluation of the ShuntingYard instance.
   */
  implicit def toOptionInt(s: ShuntingYard): Option[Int] = s.apply

  /**
   * Evaluates a mathematical expression given as a string in infix notation.
   * The method uses the Shunting Yard algorithm for parsing and evaluating the expression.
   *
   * @param s the mathematical expression in infix notation represented as a string
   * @return an Option containing the evaluated result as an integer, or None if the evaluation fails
   */
  def evaluate(s: String): Option[Int] =
    new ShuntingYardParser().parseTokens(s).foldLeft(ShuntingYard.empty)((s, x) => s(x))
}

/**
 * A custom exception class to handle errors specific to the Shunting Yard algorithm.
 *
 * This exception is meant to be thrown when invalid input or an error condition occurs
 * during the execution of the Shunting Yard algorithm.
 *
 * @param str the error message describing the nature of the exception.
 */
case class ShuntingYardException(str: String) extends Exception(str)