/*
 * Copyright (c) 2019. Phasmid Software
 */

package edu.neu.coe.csye7200.fp.list

import scala.annotation.tailrec

/**
  * Definitions for an algebraic list: i.e. we don't always show the usual name of methods.
  * NOTE: also that we don't usually use tail-recursive methods because we are trying for simplicity here.
  *
  * @tparam A the underlying (covariant) type of the List
  */
trait List[+A] extends (Int => Option[A]) {
  override def toString: String = {
    @tailrec def tos(as: List[A], r: StringBuffer): CharSequence = as match {
      case Nil => r
      case h Cons t => tos(t, r.append((if (r.length > 1) ", " else "") + s""""$h""""))
    }

    tos(this, new StringBuffer("(")) + ")"
  }

  def ++[B >: A](x: List[B]): List[B]

  def :+[B >: A](x: B): List[B]

  def +:[B >: A](x: B): List[B]

  def length: Int

  def isEmpty: Boolean

  def x0: Boolean

  def x1: Int

  def x2: A

  def x3: List[A]

  def x3a: Option[A]

  def x4(x: Int): A

  def x5(f: A => Boolean): List[A]

  def x6(f: A => Boolean): Option[A]

  def x7[B](f: A => B): List[B]

  def x8[B](f: A => List[B]): List[B]

  def x9(f: A => Unit): Unit

  def foldLeft[B](z: B)(f: (B, A) => B): B

  def sum[B >: A : Numeric]: B = foldLeft(implicitly[Numeric[B]].zero)(implicitly[Numeric[B]].plus(_, _))

  def sumByIteration[B >: A : Numeric] = {
    var result = implicitly[Numeric[B]].zero
    x9(a => result = implicitly[Numeric[B]].plus(result, a))
    result
  }
}

case object Nil extends List[Nothing] {
  def ++[B](x: List[B]): List[B] = x

  def :+[B >: Nothing](x: B): List[B] = List(x)

  def +:[B >: Nothing](x: B): List[B] = :+(x)

  def length: Int = 0

  def isEmpty: Boolean = true

  def x0: Boolean = true

  def x1: Int = 0

  def x2: Nothing = throw new Exception("logic error")

  def x3: List[Nothing] = throw new Exception("logic error")

  def x3a: Option[Nothing] = None

  def x4(x: Int): Nothing = throw new IndexOutOfBoundsException(s"cannot index empty list")

  def apply(v1: Int): Option[Nothing] = None

  def x5(f: Nothing => Boolean): List[Nothing] = Nil

  def x6(f: Nothing => Boolean): Option[Nothing] = None

  def x7[B](f: Nothing => B): List[B] = Nil

  def x8[B](f: Nothing => List[B]): List[B] = Nil

  def x9(f: Nothing => Unit): Unit = ()

  def foldLeft[B](z: B)(f: (B, Nothing) => B): B = z
}

case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  def equals[B >: A](bs: List[B]): Boolean = bs match {
    case Nil => false
    case h Cons t => h == head && t.equals(tail)
  }

  def ++[B >: A](bs: List[B]): List[B] = bs match {
    case Nil => this
    case h Cons t => Cons[B](head, tail :+ h) ++ t
  }

  def :+[B >: A](x: B): List[B] = Cons[B](head, tail :+ x)

  def +:[B >: A](x: B): List[B] = Cons[B](x, this)

  def length: Int = 1 + tail.length

  def isEmpty: Boolean = false

  def x0: Boolean = false

  def x1: Int = 1 + tail.x1

  def x2: A = head

  def x3: List[A] = tail

  def x3a: Option[A] = Some(head)

  // This corresponds to get
  def x4(idx: Int): A = if (idx == 0) head else tail.x4(idx - 1)
//  {
//    @tailrec def inner(x: Int, as: List[A]): A =
//      if (x == 0) as.x2
//      else as match {
//        case Nil => throw new IndexOutOfBoundsException(s"index out of bounds: $idx")
//        case _ Cons t => inner(x - 1, t)
//      }
//
//    if (idx >= 0) inner(idx, this) else throw new IndexOutOfBoundsException(s"index out of bounds: $idx")
//  }

  // NOTE that get and apply are reversed compared with the Scala List trait.
  def apply(idx: Int): Option[A] = if (idx == 0) Some(head) else tail(idx - 1)
//  {
//    @tailrec def inner(x: Int, as: List[A]): Option[A] = as match {
//      case Nil => None
//      case h Cons t => if (x == 0) Some(h) else inner(x - 1, t)
//    }
//
//    if (idx >= 0) inner(idx, this) else None
//  }

  def x5(f: A => Boolean): List[A] = {
    val tff = tail x5 f
    if (f(head)) head +: tff else tff
  }

  def x6(f: A => Boolean): Option[A] = if (f(head)) Some(head) else tail x6 f

  def x7[B](f: A => B): List[B] = f(head) +: (tail x7 f)

  def x8[B](f: A => List[B]): List[B] = f(head) ++ (tail x8 f)

  def x9(f: A => Unit): Unit = { f(head); tail.x9(f) }

  def foldLeft[B](z: B)(f: (B, A) => B): B = tail.foldLeft(f(z, head))(f)
}

abstract class Counter[-A] extends (A => Int)

object List {
  type IntList = List[Int]

  def sum(ints: IntList): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
