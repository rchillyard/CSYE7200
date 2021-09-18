package edu.neu.coe.csye7200.fp

import edu.neu.coe.csye7200.fp.Par.sum

trait Par[T] {
  def get: T

  def unit(t: T): Par[T]
}

object Par {
  def apply[T](t: T): Par[T] = new Par[T] {
    def get: T = t

    def unit(u: T): Par[T] = Par.apply(u)
  }

  def sum(xs: IndexedSeq[Int]): Int = xs.size match {
    case 0 | 1 => xs.headOption getOrElse 0
    case n =>
      val (l, r) = xs.splitAt(xs.length / 2)
      val sumL = Par(sum(l))
      val sumR = Par(sum(r))
      sumL.get + sumR.get
  }

}

object TestPar extends App {
  println(sum(IndexedSeq(1, 2, 3)))

}
