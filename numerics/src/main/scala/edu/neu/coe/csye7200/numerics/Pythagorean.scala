package edu.neu.coe.csye7200.numerics

import edu.neu.coe.csye7200.numerics.Pythagorean.{isSquare, square}
import edu.neu.coe.csye7200.numerics.Rational.{bigOne, gcd}

import scala.collection.mutable

case class Pythagorean(x: Long, y: Long) {
  def valid: Boolean = isSquare(square(x) + square(y)) // NOTE redundant
}

object Pythagorean {
  def apply(t: (Long, Long)): Pythagorean = apply(t._1, t._2)

  def makeList(x: Int): LazyList[Pythagorean] = makeList(LazyList.from(x).map(_.toLong).flatMap(makePythagoreanPairs))

  def makeList(ts: LazyList[(Long, Long)]): LazyList[Pythagorean] = ts filter isValid map apply

  def makePairs(x: Long): LazyList[(Long, Long)] = LazyList.continually(x) zip (1L until x) map (t => t.swap)

  def makePythagoreanPair(x: Long, y: Long): (Long, Long) = (square(y) - square(x), 2 * x * y)

  def makePythagoreanPairs(x: Long): LazyList[(Long, Long)] = makePairs(x) filter coprime map (makePythagoreanPair _).tupled

  def coprime(t: (Long, Long)): Boolean = gcd(t._2, t._1) == bigOne && (t._1 % 2 == 0 || t._2 % 2 == 0)

  def square(x: Long): Long = x * x

  def isSquare(x: Long): Boolean = square(math.round(math.sqrt(x))) == x

  def isValid(t: (Long, Long)): Boolean = isSquare(square(t._1) + square(t._2))

  def isPythagorean(t: (Long, Long)): Boolean = pythagoreans.getOrElseUpdate(t, isValid(t))

  private val pythagoreans = new mutable.HashMap[(Long, Long), Boolean]()
}
