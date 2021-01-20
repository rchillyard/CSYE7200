/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.fp.greedy

object Zeckendorf extends (Long => List[Long]) {

  val greedy: Greedy[Long, List[Long]] = Greedy(Fibonacci.getLargest, _ - _, _ +: _, _ <= 0)

  def apply(x: Long): List[Long] = greedy.apply(x, Nil)
}
