package edu.neu.coe.csye7200.shuntingyard

import scala.language.implicitConversions

/**
  * Case class which supports the Shunting Yard algorithm of Dijkstra.
  *
  * @param valueStack    a Stack[Int].
  * @param operatorStack a Stack[Operator].
  */
case class ShuntingYard(valueStack: Stack[Int], operatorStack: Stack[Operator]) extends (ShuntingYardParser#Token => ShuntingYard) {

  /**
    * Method to transform this ShuntingYard according to the token value t.
    *
    * @param t a ShuntingYardParse#Token.
    * @return (usually) a new ShuntingYard.
    */
  override def apply(t: ShuntingYardParser#Token): ShuntingYard = t match {
    case Right(Left(o)) => ShuntingYard(valueStack, operatorStack.push(o))
    case Right(Right(x)) => ShuntingYard(valueStack.push(x), operatorStack)
    case Left(Parenthesis(true)) => this // for now, we ignore left parenthesis
    case Left(Parenthesis(false)) =>
      // When we process a right-parenthesis,
      // we pop the top operator and the two top values from their respective stacks,
      // apply the operator and push the resulting value onto the value stack.
      /*SOLUTION*/
    ???
    /*END*/
  }

  /**
    * Method to evaluate this ShuntingYard that has only one element in its value stack.
    *
    * @return an optional Int value. If this ShuntingYard has exactly one value and no operators,
    *         then we return that value wrapped in Some; otherwise we return None.
    */
  def apply: Option[Int] = (valueStack, operatorStack) match {
    case (ListStack(x :: Nil), ListStack(Nil)) => Some(x)
    case _ => None
  }

  override def toString(): String = s"ShuntingYard($valueStack,$operatorStack)"
}

/**
  * Companion object to ShuntingYard.
  */
object ShuntingYard {
  def apply: ShuntingYard = new ShuntingYard(Stack[Int], Stack[Operator])

  implicit def toOptionInt(s: ShuntingYard): Option[Int] = s.apply

  def evaluate(s: String): Option[Int] =
    new ShuntingYardParser().parseTokens(s).foldLeft(ShuntingYard.apply)((s, x) => s(x))
}

case class ShuntingYardException(str: String) extends Exception(str)