package edu.neu.coe.csye7200.assthw

import edu.neu.coe.csye7200.assthw.FizzBuzz.fizzBuzz
import scala.collection.mutable
import scala.collection.mutable.Queue

/**
 * Executes the FizzBuzz logic for integers from 1 to 100, printing the results to standard output.
 * Numbers divisible by 3 are substituted with "Fizz", divisible by 5 with "Buzz", and divisible by both with "FizzBuzz".
 *
 * @return Unit as the primary purpose of this method is to produce output.
 */
@main def fizzBuzz0(): Unit =
  def fizzBuzz(x: Int): Unit =
    if x % 3 == 0 && x % 5 == 0 then println("FizzBuzz")
    else if x % 3 == 0 then println("Fizz")
    else if x % 5 == 0 then println("Buzz")
    else println(x)
  1 to 100 foreach fizzBuzz

@main def fizzBuzz1(): Unit =
  for (x <- 1 to 100) println(fizzBuzz(x))

def fizzBuzz(x: Int): String =
  (x % 3 == 0, x % 5 == 0) match {
    case (true, true) => "FizzBuzz"
    case (true, _) => "Fizz"
    case (_, true) => "Buzz"
    case _ => x.toString
  }

@main def fizzBuzz2(): Unit = {
  val strings = for (x <- 1 to 100) yield fizzBuzz(x)
  println(strings mkString("", "\n", ""))
}

/**
 * Represents a factor and provides utilities for checking multiples and extracting quotient values.
 *
 * @param f the factor to be used for checking divisibility and computing quotients
 */
case class Factor(f: Int):
  def isMultiple(x: Int): Boolean = x % f == 0
  def unapply(x: Int): Option[Int] =
    if isMultiple(x) then Some(x / f) else None

/**
 * An implementation of FizzBuzz that uses pattern-matching rather than (redundant) if clauses.
 *
 * This method uses a rather advance pattern-matching trick: declaring our own unapply method for Factor.
 */
object FizzBuzz:
  private val dividesBy3 = Factor(3)
  private val dividesBy5 = Factor(5)
  private val dividesBy3And5 = Factor(15)

  def fizzBuzz(x: Int): String =
    require(x > 0, s"x must be positive but is $x")
    x match
      case dividesBy3And5(_) => "FizzBuzz"
      case dividesBy3(_)     => "Fizz"
      case dividesBy5(_)     => "Buzz"
      case _                 => x.toString

@main def fizzBuzz3(): Unit =
  println((1 to 100).map(FizzBuzz.fizzBuzz).mkString("\n"))
