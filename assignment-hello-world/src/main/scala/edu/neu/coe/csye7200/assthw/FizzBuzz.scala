package edu.neu.coe.csye7200.assthw

import edu.neu.coe.csye7200.assthw.FizzBuzz.fizzBuzz

/**
 * Executes the FizzBuzz logic for integers from 1 to 100, printing the results to standard output.
 * Numbers divisible by 3 are substituted with "Fizz", divisible by 5 with "Buzz", and divisible by both with "FizzBuzz".
 *
 * @return Unit as the primary purpose of this method is to produce output.
 */
@main def fizzBuzzMain(): Unit = {
  val strings = for (x <- 1 to 100) yield fizzBuzz(x)
  println(strings mkString("", "\n", ""))
}

/**
 * An implementation of FizzBuzz that uses pattern-matching rather than (redundant) if clauses.
 *
 * This method uses a rather advance pattern-matching trick: declaring our own unapply method for Factor.
 */
object FizzBuzz:
  /**
   * Implements the FizzBuzz logic for a given integer, returning "Fizz" if divisible by 3,
   * "Buzz" if divisible by 5, "FizzBuzz" if divisible by both, or the number as a string otherwise.
   *
   * @param x the integer to be evaluated
   * @return a string representing the FizzBuzz result for the input integer
   */
  def fizzBuzz(x: Int): String =
    lazy val dividesBy3 = Factor(3)
    lazy val dividesBy5 = Factor(5)
    lazy val dividesBy3And5 = Factor(3 * 5)
    x match {
      case _ if x <= 0 => throw new IllegalArgumentException("x must be positive")
      case dividesBy3And5(_) => "FizzBuzz"
      case dividesBy3(_) => "Fizz"
      case dividesBy5(_) => "Buzz"
      case _ => x.toString
    }

/**
 * Case class to model the concept of factors.
 *
 * @param f a factor.
 */
case class Factor(f: Int):
  /**
   * Determine whether f is a factor of x.
   *
   * @param x a potential multiple of f.
   * @return true if f is a factor of x.
   */
  def isMultiple(x: Int): Boolean = x % f == 0

  /**
   * Unapply method for this Factor.
   *
   * @param x a candidate multiple of f.
   * @return Some(quotient) if x is a multiple of f; otherwise, None.
   */
  def unapply(x: Int): Option[Int] = if (isMultiple(x)) Some(x / f) else None
