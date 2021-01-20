/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.fp.greedy

/**
  * This object provides a LazyList of Fibonacci numbers.
  * In Scala, a LazyList is both lazy and memoized.
  */
object Fibonacci {
  /**
    * A stream of Fibonacci numbers of form LazyList[Long].
    * This formulation is due to http://www.luigip.com/?p=200
    */
  val fibonacci: LazyList[Long] = 0L #:: fibonacci.scanLeft(1L)(_ + _)

  /**
    * A stream of Fibonacci numbers of form LazyList[BigInt].
    * This formulation is due to http://www.luigip.com/?p=200
    */
  val fibonacciBigInt: LazyList[BigInt] = BigInt(0) #:: fibonacciBigInt.scanLeft(BigInt(1))(_ + _)

  /**
    * A stream of Fibonacci numbers of form LazyList[BigInt].
    * This uses a less elegant (but more comprehensible perhaps) formulation.
    */
  val fibonacciAlt: LazyList[BigInt] = BigInt(0) #:: BigInt(1) #:: fibonacciAlt.zip(fibonacciAlt.tail).map(n => n._1 + n._2)

//  println(fibonacciBigInt take 100 toList)

  /**
    * Method to yield that largest Fibonacci number which is less than or equal to x
    *
    * @param x the given value
    * @return the largest Fibonacci number less than or equal to x
    */
  def getLargest(x: Long): Long = fibonacci.takeWhile(_ <= x).last
}
