/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.lab99.scala99

import scala.annotation.tailrec

object P00 {
  def flatten[X](xss: List[List[X]]): List[X] = {
    @tailrec
    def inner(r: List[X], wss: List[List[X]]): List[X] = wss match {
      case Nil => r
      case h :: t => inner(r ++ h, t)
    }

    inner(Nil, xss)
  }

  def fill[X](n: Int)(x: X): List[X] = {
    @tailrec
    def inner(r: List[X], l: Int): List[X] = if (l <= 0) r else inner(r :+ x, l - 1)

    inner(Nil, n)
  }
}

object P01 {

  @tailrec
  def last[X](xs: List[X]): X =
  // TO BE IMPLEMENTED 
  last(Nil)
  // END
}

object P02 {

  @tailrec
  def penultimate[X](xs: List[X]): X =
  // TO BE IMPLEMENTED 
  penultimate(Nil)
  // END
}

object P03 {

  @tailrec
  def kth[X](k: Int, xs: List[X]): X =
  // TO BE IMPLEMENTED 
  kth(0,Nil)
  // END
}

object P04 {

  def length[X](xs: List[X]): Int =
  // TO BE IMPLEMENTED 
  0
  // END
}

object P05 {

  def reverse[X](xs: List[X]): List[X] =
  // TO BE IMPLEMENTED 
  Nil
  // END
}

object P06 {

  def isPalindrome[X](ys: List[X]): Boolean =
    // TO BE IMPLEMENTED 
  false
    // END
}

object P07 {

  type ListAny = List[Any]

  def flatten(xs: ListAny): ListAny =
  // TO BE IMPLEMENTED 
    ???
}

object P08 {

  def compress[X](xs: List[X]): List[X] =
  // TO BE IMPLEMENTED 
    ???
}

object P09 {

  def pack[X](xs: List[X]): List[List[X]] = {
  // TO BE IMPLEMENTED 
    ???
  }
}

object P10 {

  def encode[X](xs: List[X]): List[(Int, X)] =
  // TO BE IMPLEMENTED 
    ???
}

object P11 {

  def encodeModified[X](xs: List[X]): List[Any] =
  // TO BE IMPLEMENTED 
    ???
}

object P12 {

  def decode[X](xIs: List[(Int, X)]): List[X] =
  // TO BE IMPLEMENTED 
    ???
}

object P13 {

  def encodeDirect[X](xs: List[X]): List[(Int, X)] = {
  // TO BE IMPLEMENTED 
    ???
  }
}

object P14 {

  def duplicate[X](xs: List[X]): List[X] = {
  // TO BE IMPLEMENTED 
    ???
  }
}

object P15 {

  def duplicateN[X](n: Int, xs: List[X]): List[X] = {
  // TO BE IMPLEMENTED 
    ???
  }
}


object P16 {

  def sum[X: Numeric](xs: List[X]): X = {
    val nx: Numeric[X] = implicitly[Numeric[X]]

    @tailrec
    def inner(result: X, work: List[X]): X = work match {
      case Nil => result
      case h :: t => inner(nx.plus(result, h), t)
    }

    inner(nx.zero, xs)
  }
}