/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.fp.greedy

/**
  * This object provides a LazyList of Pell numbers.
  * In Scala, a LazyList is both lazy and memoized.
  */
object Pell {
  /**
    * A stream of Pell numbers of form LazyList[Long].
    * This formulation is adapted from http://www.luigip.com/?p=200
    */
  val pell: LazyList[Long] = 0L #:: pell.scanLeft(1L)(_ * 2 + _)

  def apply(n: Int): Long = pell(n)

  /**
    * A stream of Pell numbers of form LazyList[BigInt].
    * This formulation is adapted from http://www.luigip.com/?p=200
    */
  val pellBigInt: LazyList[BigInt] = BigInt(0) #:: pellBigInt.scanLeft(BigInt(1))(_ + _)

  /**
    * A stream of Pell numbers of form LazyList[BigInt].
    * This uses a less elegant (but more comprehensible perhaps) formulation.
   */
  val pellAlt: LazyList[BigInt] = BigInt(0) #:: BigInt(1) #:: pellAlt.zip(pellAlt.tail).map(n => n._1 + n._2)

  /**
   * Method to yield that largest Pell number which is less than or equal to x
   *
   * @param x the given value
   * @return the largest Pell number less than or equal to x
   */
  def getLargest(x: Long): Long = pell.takeWhile(_ <= x).last

  def bad(n: Int): Long = {
    if (n <= 2) return n
    2 * apply(n - 1) + apply(n - 2)
  }
}
