/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.rules


import edu.neu.coe.csye7200.rules.Orderable.{OrderableDouble, OrderableInt, OrderableLong}

import scala.util.{Failure, Success, Try}

/**
  * Type class Valuable. Perhaps the name "Arithmetical" would be better as this trait encompasses many arithmetic
  * operators.
  *
  * This combination of trait Valuable and implicit objects comprises the "type class" Valuable.
  * It is loosely based on Numeric but has more method definitions and is therefore much more useful for parsing and evaluating expressions.
  * In particular, it provides a fromString method which, to me, seems sorely lacking in Numeric.
  * The reason that it does not extend Numeric is that most of the method result in a Try[X] rather than an X.
  *
  * Created by scalaprof on 6/5/16.
  *
  * CONSIDER move this into values package
  */
trait Valuable[X] extends Orderable[X] {
  /**
    * The addition method.
    *
    * @param x an operand
    * @param y another operand
    * @return the sum of x and y as a Try[X]
    */
  def plus(x: X, y: X): Try[X]

  /**
    * The subtraction method.
    *
    * @param x the first operand
    * @param y the second operand
    * @return x-y as a Try[X]
    */
  def minus(x: X, y: X): Try[X]

  /**
    * The negation method.
    *
    * @param x the operand
    * @return -x as a Try[X]
    */
  def negate(x: X): Try[X]

  /**
    * The multiplication method.
    *
    * @param x an operand
    * @param y another operand
    * @return the product of x and y as a Try[X]
    */
  def times(x: X, y: X): Try[X]

  /**
    * The division method.
    *
    * @param x the numerator
    * @param y the denominator
    * @return the quotient of x and y as a Try[X]
    */
  def div(x: X, y: X): Try[X]

  /**
    * The reciprocal method.
    *
    * @param x an operand
    * @return 1/x as a Try[X]
    */
  def invert(x: X): Try[X]

  /**
    * The exponentiation method.
    *
    * @param x the base
    * @param y the exponent operand
    * @return x to the power of y as a Try[X]
    */
  def pow(x: X, y: X): Try[X]

  /**
    * The conversion method from an Int to an X
    *
    * @param x an operand
    * @return x as a Try[X]
    */
  def fromInt(x: Int): Try[X]

  /**
    * The identity for multiplication.
    *
    * @return 1 as an X
    */
  def one: X

  /**
    * The anadic function method for arbitrary functions of Double operand(s)
    *
    * @param f the function
    * @return f() as a Try[X]
    */
  def function0(f: () => Double): Try[X]

  /**
    * The monadic function method for arbitrary functions of Double operand(s)
    *
    * @param f the function
    * @param x an operand
    * @return f(x) as a Try[X]
    */
  def function1(f: (Double) => Double)(x: X): Try[X]

  /**
    * The dyadic function method for arbitrary functions of Double operand(s)
    *
    * @param f the function
    * @param x the first operand
    * @param y the second operand
    * @return f(x,y) as a Try[X]
    */
  def function2(f: (Double, Double) => Double)(x: X, y: X): Try[X]
}

object Valuable {
  def apply[X: Valuable]: Valuable[X] = implicitly[Valuable[X]]

  trait ValuableDouble extends OrderableDouble with Valuable[Double] {
    def plus(x: Double, y: Double) = Try(x + y)

    def minus(x: Double, y: Double) = Try(x - y)

    def negate(x: Double): Try[Double] = minus(zero, x)

    def times(x: Double, y: Double) = Try(x * y)

    def div(x: Double, y: Double) = Try(x / y)

    def invert(x: Double): Try[Double] = div(one, x)

    def pow(x: Double, y: Double): Try[Double] = Try(math.pow(x, y))

    def fromInt(x: Int) = Try(x.toDouble)

    def one = 1.0

    def function0(f: () => Double): Try[Double] = Try(f())

    def function1(f: (Double) => Double)(x: Double): Try[Double] = Try(f(x))

    def function2(f: (Double, Double) => Double)(x: Double, y: Double): Try[Double] = Try(f(x, y))
  }

  implicit object ValuableDouble extends ValuableDouble

  trait ValuableInt extends OrderableInt with Valuable[Int] {
    def plus(x: Int, y: Int) = Try(x + y)

    def minus(x: Int, y: Int) = Try(x - y)

    def negate(x: Int): Try[Int] = minus(zero, x)

    def times(x: Int, y: Int) = Try(x * y)

    def div(x: Int, y: Int) = Try(if (x % y == 0) x / y else throw new ValuableException("integer division leaves remainder"))

    def invert(x: Int) = Failure(new ValuableException("cannot invert an Int"))

    def pow(x: Int, y: Int): Try[Int] = Try(Seq.fill[Int](y)(x).product)

    def fromInt(x: Int) = Success(x)

    def one = 1

    def function0(f: () => Double): Try[Int] = Failure(new ValuableException("cannot apply an arbitrary function0 for Int"))

    def function1(f: (Double) => Double)(x: Int): Try[Int] = Failure(new ValuableException("cannot apply an arbitrary function1 for Int"))

    def function2(f: (Double, Double) => Double)(x: Int, y: Int): Try[Int] = Failure(new ValuableException("cannot apply an arbitrary function2 for Int"))
  }

  implicit object ValuableInt extends ValuableInt

  trait ValuableLong extends OrderableLong with Valuable[Long] {
    def plus(x: Long, y: Long) = Try(x + y)

    def minus(x: Long, y: Long) = Try(x - y)

    def negate(x: Long): Try[Long] = minus(zero, x)

    def times(x: Long, y: Long) = Try(x * y)

    def div(x: Long, y: Long) = Try(if (x % y == 0) x / y else throw new ValuableException("integer division leaves remainder"))

    def invert(x: Long) = Failure(new ValuableException("cannot invert an Long"))

    def pow(x: Long, y: Long): Try[Long] = Try(Seq.fill[Int](y.toInt)(x.toInt).product)

    def fromInt(x: Int) = Success(x.toLong)

    def one = 1

    def function0(f: () => Double): Try[Long] = Failure(new ValuableException("cannot apply an arbitrary function0 for Long"))

    def function1(f: (Double) => Double)(x: Long): Try[Long] = Failure(new ValuableException("cannot apply an arbitrary function1 for Long"))

    def function2(f: (Double, Double) => Double)(x: Long, y: Long): Try[Long] = Failure(new ValuableException("cannot apply an arbitrary function2 for Long"))
  }

  implicit object ValuableLong extends ValuableLong

  class ValuableException(s: String) extends Exception(s)

}
