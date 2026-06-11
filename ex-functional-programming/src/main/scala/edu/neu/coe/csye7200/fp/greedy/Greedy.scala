/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.fp.greedy

import scala.annotation.tailrec

case class Greedy[T, R](fGreedy: T => T, fAdjust: (T, T) => T, fResult: (T, R) => R, fDone: T => Boolean) extends ((T, R) => R) {
  override def apply(t: T, r: R): R = {

    @tailrec def inner(_t: T, _r: R): R =
      if (fDone(_t)) _r
      else {
        val greedy = fGreedy(_t)
        inner(fAdjust(_t, greedy), fResult(greedy, _r))
      }

    inner(t, r)
  }
}
