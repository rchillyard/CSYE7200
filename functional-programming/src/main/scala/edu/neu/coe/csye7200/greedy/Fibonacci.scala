/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.greedy

/**
  * This object provides a Stream of Fibonacci numbers.
  * In Scala, a Stream is both lazy and memoized.
  */
object Fibonacci {
  /**
    * A stream of Fibonacci numbers.
    * This formulation is due to http://www.luigip.com/?p=200
    */
  val stream: Stream[Long] = 0L #:: stream.scanLeft(1L)(_ + _)

  /**
    * Method to yield that largest Fibonacci number which is less than or equal to x
    *
    * @param x the given value
    * @return the largest Fibonacci number less than or equal to x
    */
  def getLargest(x: Long): Long = stream.takeWhile(_ <= x).last
}
