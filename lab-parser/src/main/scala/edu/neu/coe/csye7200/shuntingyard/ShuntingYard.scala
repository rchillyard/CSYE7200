package edu.neu.coe.csye7200.shuntingyard

import scala.language.implicitConversions

/**
  * Case class which supports the Shunting Yard algorithm of Dijkstra.
 *
 * See [[https://en.wikipedia.org/wiki/Shunting_yard_algorithm]]
  *
  * @param valueStack    a Stack[Int].
  * @param operatorStack a Stack[Operator].
  */
case class ShuntingYard(valueStack: Stack[Int], operatorStack: Stack[Operator], depth: Int) extends (ShuntingYardParser#Token => ShuntingYard) {

  self =>

  /**
    * Method to transform this ShuntingYard according to the token value t.
    *
    * @param t a ShuntingYardParse#Token.
    * @return (usually) a new ShuntingYard.
    */
  override def apply(t: ShuntingYardParser#Token): ShuntingYard = t match {
    case Right(Left(o)) => ShuntingYard(valueStack, operatorStack.push(o), depth)
    case Right(Right(x)) => ShuntingYard(valueStack.push(x), operatorStack, depth)
    case Left(Parenthesis(true)) => ShuntingYard(valueStack, operatorStack, depth + 1) // for now, we ignore left parenthesis
    case Left(Parenthesis(false)) => evaluate(depth - 1)
  }

  /**
    * Method to evaluate this ShuntingYard as an optional Int.
    * The depth of this ShuntingYard must be zero, otherwise None will be returned.
    *
    * @return an optional Int value.
    */
  def apply: Option[Int] = self match {
    // Terminating condition: If this ShuntingYard has depth of 0 and exactly one value and no operators,
    //    then we return that value wrapped in Some; otherwise we return None.
    case ShuntingYard(ListStack(x :: Nil), EmptyStack, 0) => Some(x)
    // Recursive case: If this ShuntingYard has depth of 0 and at least two values and one operator,
    //    then we recursively call apply to the evaluated version of this.
    case ShuntingYard(ListStack(_ :: _ :: _), ListStack(_ :: _), 0) => evaluate(0).apply
    // Otherwise, we must return None.
    case _ => None
  }

  private def evaluate(d: Int): ShuntingYard = {
    // When we evaluate this ShuntingYard,
    // we pop the top operator and the two top values from their respective stacks,
    // apply the operator and push the resulting value onto the value stack.
    //      ???

    // TO BE IMPLEMENTED 
???
  }

  override def toString(): String = s"ShuntingYard($valueStack,$operatorStack,$depth)"
}

/**
  * Companion object to ShuntingYard.
  */
object ShuntingYard {
  def apply: ShuntingYard = new ShuntingYard(Stack[Int], Stack[Operator], 0)

  implicit def toOptionInt(s: ShuntingYard): Option[Int] = s.apply

  def evaluate(s: String): Option[Int] =
    new ShuntingYardParser().parseTokens(s).foldLeft(ShuntingYard.apply)((s, x) => s(x))
}

case class ShuntingYardException(str: String) extends Exception(str)