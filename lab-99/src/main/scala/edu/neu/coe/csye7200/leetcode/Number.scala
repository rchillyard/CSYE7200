package edu.neu.coe.csye7200.leetcode

import scala.annotation.tailrec

/**
  * This case class represents a Number.
  * It is made up of a digit (x), and a Number which corresponds to all of the digits to the left of x.
  *
  */
sealed trait Number extends (() => BigInt) {
  def +(n: Number): Number
}

/**
  * This case class represents a Number.
  * It is made up of a digit (x), and a Number which corresponds to all of the digits to the left of x.
  *
  * @param x
  * @param nextSignificantNumber
  */
case class Number_Recursive(x: Int, nextSignificantNumber: Number) extends Number {
  /**
    * Apply method which converts this Number into a Long.
    *
    * @return a Long corresponding to the value of this Number.
    */
  override def apply(): BigInt = x + nextSignificantNumber() * BigInt(10)

  def render: String = this match {
    case Number_Recursive(z, Zero) => s"$z"
    case Number_Recursive(z, n: Number_Recursive) => s"$z -> ${n.render}"
  }

  override def toString(): String = this.render

  def +(n: Number): Number = n match {
    case Zero => this
    case _ => Number(Number_Recursive.add(Nil, this, n, 0))
  }
}

object Number_Recursive {
  def apply(x: Int): Number_Recursive = Number_Recursive(x, Zero)

  /**
    * Tail-recursive method to add two Numbers together.
    *
    * NOTE: this should really be private but is used in unit tests.
    *
    * @param r     the current sequence.
    * @param n1    first Number.
    * @param n2    second Number.
    * @param carry value of carry.
    * @return a sequence of Ints.
    */
  def add(r: Seq[Int], n1: Number, n2: Number, carry: Int): Seq[Int] = (n1, n2, carry) match {
    // Terminating condition
    case (Zero, Zero, z) => if (z > 0) r :+ z else r
    // Swap the order so that the first number is always recursive (not Zero)
    case (Zero, n, z) => add(r, n, Zero, z)
    // First number is recursive, second is zero so let's replace zero by an equivalent recursive number
    case (n, Zero, z) => add(r, n, Number_Recursive(0, Zero), z)
    // This is where we do the real work!
    case (Number_Recursive(x1, m1), Number_Recursive(x2, m2), z) =>
      val sum = x1 + x2 + z
      add(r :+ sum % 10, m1, m2, sum / 10)
  }
}

/**
  * This case class represents zero as a Number.
  */
case object Zero extends Number {
  override def apply(): BigInt = 0

  override def toString(): String = "0"

  def +(n: Number): Number = n
}

object Number {
  /**
    * Apply method to create a Number based on a sequence of Int.
    * NOTE: this method is not tail-recursive.
    *
    * @param xs a list of Ints with the least significant digit at the head.
    * @return a Number.
    */
  def apply(xs: Seq[Int]): Number = xs match {
    case Nil => Zero
    case h :: t => Number_Recursive(h, apply(t))
  }

  def apply(x: Long): Number = apply(getDigits(x))

  def apply(x: BigInt): Number = apply(getDigits(x))

  /**
    * Get the digits for a positive Long.
    *
    * @param x a Long.
    * @return a Seq[Int] beginning with the least significant digit.
    */
  def getDigits(x: Long): Seq[Int] = {
    @tailrec
    def inner(r: Seq[Int], z: Long): Seq[Int] = if (z == 0) r else inner(r :+ (z % 10).toInt, z / 10)

    inner(Nil, x)
  }

  /**
    * Get the digits for a BigInt.
    *
    * @param bigInt a BigInt.
    * @return a Seq[Int] beginning with the least significant digit.
    */
  def getDigits(bigInt: BigInt): Seq[Int] = {
    val xs = bigInt.toString.toCharArray
    val ys = for (x <- xs) yield s"$x".toInt
    ys.reverse.toList
  }

}


/**
  * This problem is one of LeetCode's Algorithm problems (Medium): https://leetcode.com/problems/add-two-numbers/
  */
object AddTwoNumbers extends App {

}
