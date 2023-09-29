/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.fp.greedy

import scala.annotation.tailrec

/**
  * This object employs a tail-recursive method to get the nth Lucas number.
  */
object Lucas {

  def apply(n: Int): Long = {
    @tailrec
    def inner(a: Long, b: Long, x: Int): Long = x match {
      case 0 => a
      case _ => inner(b, a + b, x - 1)
    }
    inner(2L, 1L, n)
  }
}