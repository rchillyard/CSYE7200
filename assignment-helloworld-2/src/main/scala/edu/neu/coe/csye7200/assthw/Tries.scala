package edu.neu.coe.csye7200.assthw

import scala.util.{Failure, Success, Try}

object Tries {
  def tryMatch[X](f: (X, X) => Boolean)(x: => X, expected: X, message: String): Try[X] =
    if (f(x, expected)) Success(x) else Failure(new Exception(s"$message: $x did not equal $expected"))

  def tryEquals[X](x: => X, expected: X, message: String): Try[X] = tryMatch[X](_ == _)(x, expected, message)

  def tryNotEquals[X](x: => X, expected: X, message: String): Try[X] = tryMatch[X](_ != _)(x, expected, message)
}