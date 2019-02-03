/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200

object P00 {
  def flatten[X](xss: Seq[Seq[X]]): Seq[X] = {
    def inner(r: Seq[X], wss: Seq[Seq[X]]): Seq[X] = wss match {
      case Nil => r
      case h :: t => inner(r ++ h, t)
    }

    inner(Nil, xss)
  }

  def fill[X](n: Int)(x: X): Seq[X] = {
    def inner(r: Seq[X], l: Int): Seq[X] = if (l <= 0) r else inner(r :+ x, l - 1)

    inner(Nil, n)
  }
}

object P01 {

  def last[X](xs: Seq[X]): X = ??? // TO BE IMPLEMENTED
}

object P02 {

  def penultimate[X](xs: Seq[X]): X = ??? // TO BE IMPLEMENTED
}

object P03 {

  def kth[X](k: Int, xs: Seq[X]): X = ??? // TO BE IMPLEMENTED
}

object P04 {

  def length[X](xs: Seq[X]): Int = {
    // TO BE IMPLEMENTED
    ???
  }
}

object P05 {

  def reverse[X](xs: Seq[X]): Seq[X] = {
    // TO BE IMPLEMENTED
    ???
  }
}

object P06 {

  // inefficient solution
  def isPalindrome[X](xs: Seq[X]): Boolean = ??? // TO BE IMPLEMENTED
}

object P07 {

  type Sequence = Seq[Any]

  def flatten(xs: Sequence): Sequence = {
    // TO BE IMPLEMENTED
    ???
  }
}

object P08 {

  def compress[X](xs: Seq[X]): Seq[X] = {
    // TO BE IMPLEMENTED
    ???
  }
}

object P09 {

  def pack[X](xs: Seq[X]): Seq[Seq[X]] = {
    // TO BE IMPLEMENTED
    ???
  }
}

object P10 {

  def encode[X](xs: Seq[X]): Seq[(Int, X)] = ??? // TO BE IMPLEMENTED
}

object P11 {

  def encodeModified[X](xs: Seq[X]): Seq[Any] = ??? // TO BE IMPLEMENTED
}

object P12 {

  def decode[X](xIs: Seq[(Int, X)]): Seq[X] = ??? // TO BE IMPLEMENTED
}

object P13 {

  def encodeDirect[X](xs: Seq[X]): Seq[(Int, X)] = {
    // TO BE IMPLEMENTED
    ???
  }
}

object P14 {

  def duplicate[X](xs: Seq[X]): Seq[X] = {
    // TO BE IMPLEMENTED
    ???
  }
}

object P15 {

  def duplicateN[X](n: Int, xs: Seq[X]): Seq[X] = {
    // TO BE IMPLEMENTED
    ???
  }
}
