package edu.neu.coe.csye7200.assthw

import scala.util.{Failure, Success, Try}

/**
 * Object containing utility methods for working with `Try` operations, 
 * including comparison and combination of `Try` instances.
 */
object Tries {
  /**
   * Attempts to match a lazily-evaluated value with an expected value using a provided comparison function.
   * Wraps the result in a Try, returning Success if the comparison succeeds or Failure if it does not.
   *
   * @param f        a binary function that takes two parameters of type X and returns a Boolean indicating whether they match
   * @param x        a lazily-evaluated expression of type X to be compared
   * @param expected the expected value of type X to compare against
   * @param message  a custom error message to include in the Failure case if the comparison fails
   * @return a Success wrapping the evaluated value if the comparison succeeds, or a Failure containing an exception with the provided message if it fails
   */
  def tryMatch[X](f: (X, X) => Boolean)(x: => X, expected: X, message: String): Try[X] =
    if (f(x, expected))
      Success(x)
    else
      Failure(new Exception(s"$message: $x did not equal $expected"))

  /**
   * Executes a computation `x` and checks if it equals the `expected` value.
   * If the values are equal, it returns a `Success` containing `x`.
   * Otherwise, it returns a `Failure` with an exception containing the provided `message`.
   *
   * @param x        the computation or value to evaluate and check against `expected`
   * @param expected the value that `x` is expected to match
   * @param message  the message included in the failure exception if the values are not equal
   * @return a `Try[X]` which is `Success(x)` if `x` equals `expected`, or a `Failure` otherwise
   */
  def tryEquals[X](x: => X, expected: X, message: String): Try[X] =
    tryMatch[X](_ == _)(x, expected, message)

  /**
   * Attempts to execute the provided value and check if it is not equal to the expected value.
   * If the provided value does not match the expected value, it returns a Success wrapping the value.
   * Otherwise, it returns a Failure with an exception containing the specified message.
   *
   * @param x        the computation to produce the value to be compared
   * @param expected the value to compare against
   * @param message  the error message to be included in the Failure if the comparison fails
   * @return a Success wrapping the value if it is not equal to the expected value, or a Failure with an exception otherwise
   */
  def tryNotEquals[X](x: => X, expected: X, message: String): Try[X] =
    tryMatch[X](_ != _)(x, expected, message)

  /**
   * Combines two `Try` instances into a single `Try` containing a tuple of their successful values.
   * If either `Try` is a failure, the result will be a failure containing the first encountered exception.
   *
   * @param xy the first `Try` instance containing a value of type `X`
   * @param yy the second `Try` instance containing a value of type `Y`
   * @return a `Try` containing a tuple `(X, Y)` if both inputs are successful,
   *         or a `Failure` if either input is a failure
   */
  def zipTry[X, Y](xy: Try[X], yy: Try[Y]): Try[(X, Y)] =
    for (x <- xy; y <- yy) yield (x, y)
}