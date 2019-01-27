/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.rules

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import edu.neu.coe.csye7200.rules.FP._

import scala.util._

/**
  * Type class Orderable.
  *
  * This combination of trait Orderable and implicit objects comprises the "type class" Orderable.
  *
  * This has been split off from Valuable by scalaprof on 7/10/16.
  *
  * CONSIDER extending Ordered instead of Ordering
  */
trait Orderable[X] extends Ordering[X] {
  /**
    * Method to introduce an X value from an X.
    * Yes, I know this seems nonsensical but it is necessary.
    *
    * @param x the value to be introduced
    * @return x as an X
    *         CONSIDER make unit return Try[X]
    */
  def unit(x: X): X

  /**
    * Method to introduce an X value from a String.
    *
    * @param s       the String from which we wish to parse an X
    * @param pattern (implicit) the pattern (template) of the String, for example, when parsing a date string, do we put year, month or day first?
    * @return a Try[X]
    */
  def fromString(s: String)(implicit pattern: String): Try[X]

  /**
    * Method to introduce an X value from a String key, given a lookup function.
    *
    * @param k the key to be looked up
    * @param f the lookup function. Typically, this will be the get function of a Map[String,X]
    * @return a Try[X]
    */
  def viaLookup(k: String, f: String => Option[X]): Try[X]

  /**
    * The identity for addition.
    *
    * @return zero as an X
    */
  def zero: X
}

object Orderable {
  def apply[X: Orderable]: Orderable[X] = implicitly[Orderable[X]]

  /**
    * Method to convert a String into an Orderable
    *
    * @param x       a String
    * @param pattern (implicit) the pattern (template) of the String, for example, when parsing a date string, do we put year, month or day first?
    * @return a Try[X]
    */
  def parse[X: Orderable](x: String)(implicit pattern: String): Try[X] = implicitly[Orderable[X]].fromString(x)

  def comparison(op: String, x: Any, y: Any): Try[Boolean] = {
    (x, y) match {
      case (i: Int, j: Int) => comparisonTyped[Int](op, Success(i), Success(j))(OrderableInt)
      case (i: LocalDate, j: LocalDate) => comparisonTyped[LocalDate](op, Success(i), Success(j))(OrderableLocalDate)
      case (i: Int, j: String) =>
        comparisonTyped[Int](op, Success(i), Try(j.toInt))(OrderableInt)
      case (i: String, j: Int) =>
        comparisonTyped[Int](op, Try(i.toInt), Success(j))(OrderableInt)
      case (i: String, j: String) =>
        // CONSIDER dates in the following
        comparisonTyped[Int](op, Try(i.toInt), Try(j.toInt))(OrderableInt) orElse comparisonTyped[String](op, Success(i), Success(j))(OrderableString)
      case (_, _) => Failure(new OrderableException(s"comparison unsupported for $x and $y"))
    }
  }

  def comparisonTyped[X: Orderable](op: String, x: Try[X], y: Try[X]): Try[Boolean] = {
    val orderable = implicitly[Orderable[X]]

    def notEqual(x: X, y: X): Boolean = !orderable.equiv(x, y)

    val cfy = op match {
      case "lt" => Success(orderable.lt _)
      case "le" => Success(orderable.lteq _)
      case "eq" => Success(orderable.equiv _)
      case "ne" => Success(notEqual _)
      case "ge" => Success(orderable.gteq _)
      case "gt" => Success(orderable.gt _)
      case _ => Failure(new OrderableException(s"cannot compare using op: $op (not supported)"))
    }
    for (cf <- cfy; b <- FP.map2(x, y)(cf)) yield b
  }

  trait OrderableInt extends Orderable[Int] {
    def unit(x: Int): Int = x

    def viaLookup(k: String, f: (String) => Option[Int]): Try[Int] = optionToTry(f(k), new OrderableException(s"$k is not defined"))

    def fromString(s: String)(implicit pattern: String): Try[Int] = Try(s.toInt)

    def zero: Int = 0

    def compare(x: Int, y: Int): Int = x.compare(y)
  }

  implicit object OrderableInt extends OrderableInt

  trait OrderableLong extends Orderable[Long] {
    def unit(x: Long): Long = x

    def viaLookup(k: String, f: (String) => Option[Long]): Try[Long] = optionToTry(f(k), new OrderableException(s"$k is not defined"))

    def fromString(s: String)(implicit pattern: String): Try[Long] = Try(s.toLong)

    def zero: Long = 0

    def compare(x: Long, y: Long): Int = x.compare(y)
  }

  implicit object OrderableLong extends OrderableLong

  trait OrderableDouble extends Orderable[Double] {
    def unit(x: Double): Double = x

    def fromString(s: String)(implicit pattern: String = "") = Try(s.toDouble)

    def viaLookup(k: String, f: String => Option[Double]): Try[Double] = optionToTry(f(k), new OrderableException(s"$k is not defined"))

    def zero = 0.0

    def compare(x: Double, y: Double): Int = x.compare(y)
  }

  implicit object OrderableDouble extends OrderableDouble

  trait OrderableString extends Orderable[String] {
    def unit(x: String): String = x

    def viaLookup(k: String, f: (String) => Option[String]): Try[String] = optionToTry(f(k), new OrderableException(s"$k is not defined"))

    def fromString(s: String)(implicit pattern: String): Try[String] = Success(s)

    def zero: String = ""

    def compare(x: String, y: String): Int = x.compareTo(y)
  }

  implicit object OrderableString extends OrderableString

  trait OrderableLocalDate extends Orderable[LocalDate] {
    def unit(x: LocalDate): LocalDate = x

    def viaLookup(k: String, f: (String) => Option[LocalDate]): Try[LocalDate] = optionToTry(f(k), new OrderableException(s"$k is not defined"))

    def fromString(s: String)(implicit pattern: String): Try[LocalDate] = Try(LocalDate.parse(s, if (pattern.isEmpty) isoFormatter else formatter(pattern)))

    def zero: LocalDate = LocalDate.now

    def compare(x: LocalDate, y: LocalDate): Int = x.compareTo(y)

    private val isoFormatter = DateTimeFormatter.ISO_LOCAL_DATE

    def formatter(s: String): DateTimeFormatter = DateTimeFormatter.ofPattern(s)
  }

  implicit object OrderableLocalDate extends OrderableLocalDate

  /**
    * The following trait is somewhat experimental... no unit tests as of now
    */
  trait OrderableBoolean extends Orderable[Boolean] {
    def unit(x: Boolean): Boolean = x

    def viaLookup(k: String, f: (String) => Option[Boolean]): Try[Boolean] = optionToTry(f(k), new OrderableException(s"$k is not defined"))

    def fromString(s: String)(implicit pattern: String): Try[Boolean] = Try(s.toBoolean)

    def zero: Boolean = false

    def compare(x: Boolean, y: Boolean): Int = x.compareTo(y)
  }

  implicit object OrderableBoolean extends OrderableBoolean

  class OrderableException(s: String) extends Exception(s)

}
