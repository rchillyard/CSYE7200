/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.rules

import scala.util._

trait Evaluable[X] {
  def evaluate: Try[X]
}

sealed trait Token[X]

case class Constant[X: Valuable](xt: Try[X]) extends Token[X] with Evaluable[X] {
  def evaluate: Try[X] = xt
}

case class Number[X: Valuable](n: String) extends Token[X] with Evaluable[X] {
  // CONSIDER move this into implicit parameter
  implicit val pattern: String = ""

  def evaluate: Try[X] = Valuable[X].fromString(n)
}

case class Monadic[X: Valuable](f: X => Try[X]) extends Token[X]

case class Dyadic[X: Valuable](f: (X, X) => Try[X]) extends Token[X]

case class Invalid[X](t: Throwable) extends Token[X] with Evaluable[X] {
  def evaluate: Try[X] = Failure(t)
}

/**
  * Created by scalaprof on 6/13/16.
  */
case class RPN[X: Valuable](stack: List[Token[X]]) extends Token[X] with Evaluable[X] {

  def push(t: Token[X]) = RPN(t :: stack)

  def evaluate: Try[X] = {
    // CONSIDER moving this into implicit parameter
    implicit val pattern: String = ""
    val (xt, xts) = inner(stack)
    if (xts.isEmpty) xt
    else Failure[X](new Exception(s"logic error: remaining values: $xts"))
  }

  private def inner(xts: List[Token[X]])(implicit pattern: String): (Try[X], List[Token[X]]) = xts match {
    case Nil =>
      (Failure(new RuleException("token list is empty")), xts)
    case Number(s) :: r0 =>
      (Valuable[X].fromString(s), r0)
    case Constant(x) :: r0 =>
      (x, r0)
    case Monadic(f) :: r0 =>
      val (xt, r1) = inner(r0)
      (for (x <- xt; z <- f(x)) yield z, r1)
    case Dyadic(f) :: r0 =>
      val (xt, r1) = inner(r0)
      val (yt, r2) = inner(r1)
      (for (x <- xt; y <- yt; z <- f(y, x)) yield z, r2)
    case Invalid(t) :: r0 =>
      (Failure(t), r0)
    case _ =>
      (Failure(new RuleException(s"token list is invalid: $xts")), xts)
  }
}

object Token {
  def apply[X: Valuable](s: String)(implicit lookup: String => Option[X]): Token[X] = {
    // CONSIDER checking orderableToken first
    val n: Valuable[X] = Valuable[X]
    val floatingR = """(-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?)""".r
    val lookupR = """\$(\w+)""".r
    s match {
      case floatingR(x, _, _, _) => Number[X](x)
      case floatingR(x, _, _) => Number[X](x)
      case floatingR(x, _) => Number[X](x)
      case floatingR(x) => Number[X](x)
      case lookupR(x) => Constant[X](FP.optionToTry(lookup(x), new Exception(s"no value for lookup: $x")))
      case "+" => Dyadic[X](n.plus)
      case "-" => Dyadic[X](n.minus)
      case "*" => Dyadic[X](n.times)
      case "/" => Dyadic[X](n.div)
      case "pow" => Dyadic[X](n.pow)
      case "chs" => Monadic[X](n.negate)
      case "inv" => Monadic[X](n.invert)
      case "abs" => Monadic[X](n.function1(math.abs)(_))
      case "atan" => Dyadic[X](n.function2(math.atan2)(_, _))
      case "exp" => Monadic[X](n.function1(math.exp)(_))
      case "cos" => Monadic[X](n.function1(math.cos)(_))
      case "sin" => Monadic[X](n.function1(math.sin)(_))
      case "acos" => Monadic[X](n.function1(math.acos)(_))
      case "asin" => Monadic[X](n.function1(math.asin)(_))
      case "cosh" => Monadic[X](n.function1(math.cosh)(_))
      case "log" => Monadic[X](n.function1(math.log)(_))
      case "log10" => Monadic[X](n.function1(math.log10)(_))
      case "max" => Dyadic[X](n.function2(math.max)(_, _))
      case "min" => Dyadic[X](n.function2(math.min)(_, _))
      case "sqrt" => Monadic[X](n.function1(math.sqrt)(_))
      case "sinh" => Monadic[X](n.function1(math.sinh)(_))
      case "tan" => Monadic[X](n.function1(math.tan)(_))
      case "tanh" => Monadic[X](n.function1(math.tanh)(_))
      case "toDegrees" => Monadic[X](n.function1(math.toDegrees)(_))
      case "toRadians" => Monadic[X](n.function1(math.toRadians)(_))
      case "pi" => Constant[X](n.function0({ () => math.Pi }))
      case _ => Invalid(new RuleException(s"invalid token: $s"))
    }
  }
}

object RPN {

  import scala.language.implicitConversions

  implicit def lookup[X](s: String): Option[X] = Map[String, X]().get(s)

  def apply[X: Valuable](): RPN[X] = apply(List[Token[X]]())

  def apply[X: Valuable](xs: Seq[String])(implicit lookup: String => Option[X]): RPN[X] = xs.foldLeft(RPN[X]())((r, x) => r.push(Token[X](x)))

  def apply[X: Valuable](w: String)(implicit lookup: String => Option[X]): RPN[X] = apply(w.split(" ").toSeq)

  def evaluate[X: Valuable](w: String)(implicit lookup: String => Option[X]): Try[X] = apply(w).evaluate
}
