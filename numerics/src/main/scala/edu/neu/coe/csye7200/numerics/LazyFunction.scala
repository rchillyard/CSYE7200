package edu.neu.coe.csye7200.numerics

/**
  * This abstract class is the base class for various functions used by LazyNumber algebra.
  * It's not that the functions are lazy, it's that they are used by the LazyNumber module.
  *
  * @author scalaprof
  */

abstract class LazyFunction[X: Numeric] extends (X => X) {
  /**
    * Compose two functions, such that, when applied, parameter f is applied first
    */
  def composeX(f: X => X): X => X =
    if (isInstanceOf[Known[X]])
      LazyFunction.merge(this.asInstanceOf[Known[X]], f) match {
        case Some(g) => g
        case None => Composed(this, f)
      }
    else
      Composed(this, f)

  override def compose[A](g: A => X): A => X = composeX(g.asInstanceOf[X => X]).asInstanceOf[A => X]
}

abstract class KnownDifferentiableFunction[X: Numeric](name: String, g: X => X, ds: X => Double*) extends DifferentiableFunction[X](g, ds: _*) {
  override def toString: String = name
}

abstract class DifferentiableFunction[X: Numeric](g: X => X, ds: X => Double*) extends DiFuncBase[X](g, ds: _*) with (X => X) {
  def apply(x: X): X = g(x)
}

abstract class Known[X: Numeric](name: String) extends LazyFunction[X] {
  override def toString: String = name
}

case class Identity[X: Numeric]() extends Known[X]("Identity") {
  def apply(x: X): X = x
}

// This (or something like it) is required if we have filter in LazyNumber
//case class NoFunction[X : Numeric]() extends Known[X]("NoFunction") {
//    def apply(x: X): X = implicitly[Numeric[X]].zero
//}

/**
  * the order of the parameters is significant. The result of applying this function is:
  * f(g(x))
  */
case class Composed[X: Numeric](f: X => X, g: X => X) extends LazyFunction[X] {
  override def toString = s"$f($g(_))"

  def apply(x: X): X = f(g(x))
}

case class ComposedDifferentiable[X: Numeric](g1: DiFunc[X], g2: DiFunc[X]) extends LazyFunction[X] with DiFunc[X] {
  override def toString = s"$g1($g2(_))"

  def arity: Int = if (g1.arity == g2.arity) g1.arity else throw new UnsupportedOperationException(s"composed differentiable function with different arities: ${g1.arity}, ${g2.arity}")

  // This is the so-called "Chain Rule" of differentiation
  def df_dx(i: Int): X => Double = { x => g1.df_dx(i)(g2.f(x)) * g2.df_dx(i)(x) }

  // TODO check the order here
  def f: X => X = g1.f.compose(g2.f)

  def apply(x: X): X = //g1.f(g2.f(x))
    f.apply(x)
}

/**
  * the order of the parameters is significant. The result of applying this function is:
  * g(f(x))
  */
case class AndThen[X: Numeric](f: X => X, g: X => X) extends LazyFunction[X] {
  override def toString = s"$f andThen $g"

  def apply(x: X): X = g(f(x))
}

case class Sum[X: Numeric](y: X) extends Known[X](s"add $y") {
  def apply(x: X): X = implicitly[Numeric[X]].plus(x, y)
}

case class Product[X: Numeric](y: X) extends Known[X](s"times $y") {
  def apply(x: X): X = implicitly[Numeric[X]].times(x, y)
}

case class ExpDifferentiable[X: Numeric]() extends KnownDifferentiableFunction[X](s"exp", Exp[X](), { x => math.exp(implicitly[Numeric[X]].toDouble(x)) })

case class Exp[X: Numeric]() extends Known[X]("exp") {
  def apply(x: X): X = math.exp(implicitly[Numeric[X]].toDouble(x)).asInstanceOf[X]
}

// Arbitrary function that is named for debugging purposes only
case class Named[X: Numeric](name: String, f: X => X) extends LazyFunction[X] {
  override def toString = s"$name"

  def apply(x: X): X = f(x)
}

object LazyFunction {
  def merge[X: Numeric](f: Known[X], g: X => X): Option[X => X] = {
    val q = implicitly[Numeric[X]]
    f match {
      case Sum(a) => g match {
        case Sum(b) => val r = q.plus(a, b); Some(if (r == q.zero) Identity() else Sum(r));
        case Identity() => Some(f);
        case _ => None
      }
      case Product(a) => g match {
        case Product(b) => val r = q.times(a, b); Some(if (r == q.one) Identity() else Product(r));
        case Identity() => Some(f);
        case _ => None
      }
      case Identity() => Some(g)
      //      case _ => g match { case Identity() => Some(f); case _ => None}
    }
  }

  implicit object IntLazyFunction extends Identity[Int]

  implicit object RationalLazyFunction extends Identity[Rational]

  implicit object DoubleLazyFunction extends Identity[Double]

  implicit object FuzzyLazyFunction extends Identity[Fuzzy]

}
