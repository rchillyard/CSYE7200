/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.scala99

import scala.annotation.tailrec

object P00 {
  def flatten[X](xss: List[List[X]]): List[X] = {
    @scala.annotation.tailrec
    def inner(r: List[X], wss: List[List[X]]): List[X] = wss match {
      case Nil => r
      case h :: t => inner(r ++ h, t)
    }

    inner(Nil, xss)
  }

  def fill[X](n: Int)(x: X): List[X] = {
    @scala.annotation.tailrec
    def inner(r: List[X], l: Int): List[X] = if (l <= 0) r else inner(r :+ x, l - 1)

    inner(Nil, n)
  }
}

object P01 {

  @scala.annotation.tailrec
  def last[X](xs: List[X]): X = /*SOLUTION*/ xs match {
    case x :: Nil => x
    case _ :: t => last(t)
    case _ => throw new NoSuchElementException
  }/*END*/
}

object P02 {

  @scala.annotation.tailrec
  def penultimate[X](xs: List[X]): X = /*SOLUTION*/ xs match {
    case x :: _ :: Nil => x
    case _ :: t => penultimate(t)
    case _ => throw new NoSuchElementException
  }/*END*/
}

object P03 {

  @scala.annotation.tailrec
  def kth[X](k: Int, xs: List[X]): X = /*SOLUTION*/ (k, xs) match {
    case (0, x :: _) => x
    case (n, _ :: t) => kth(n - 1, t)
    case (_, _) => throw new NoSuchElementException
  }/*END*/
}

object P04 {

  def length[X](xs: List[X]): Int = {
    @tailrec
    def inner(result: Int, _xs: List[X]): Int = {
      _xs match {
        case Nil => result
        case _ :: t => inner(result + 1, t)
      }
    }
    inner(0, xs)
  }
}

object P05 {

  def reverse[X](xs: List[X]): List[X] = {
    //SOLUTION
    @scala.annotation.tailrec
    def inner(r: List[X], _xs: List[X]): List[X] =
      _xs match {
        case Nil => r
        case h :: t => inner(h +: r, t)
      }

    inner(Nil, xs)
    //END
  }
}

object P06 {

  // inefficient solution
  def isPalindrome[X](xs: List[X]): Boolean = /*SOLUTION*/ xs == P05.reverse(xs) /*END*/
}

object P07 {

  type ListAny = List[Any]

  def flatten(xs: ListAny): ListAny = {
    //SOLUTION
    @scala.annotation.tailrec
    def inner(r: ListAny, w: ListAny): ListAny = w match {
      case Nil => r
      case h :: t =>
        h match {
          case sequence: ListAny => inner(r, sequence ++ t)
          case _ => inner(r :+ h, t)
        }
    }

    inner(Nil, xs)
    //END
  }
}

object P08 {

  def compress[X](xs: List[X]): List[X] = {
    //SOLUTION
    @scala.annotation.tailrec
    def inner(r: List[X], w: List[X], xo: Option[X]): List[X] = w match {
      case Nil => r
      case h :: t => xo match {
        case Some(x) => h match {
          case `x` => inner(r, t, xo)
          case _ => inner(r, w, None)
        }
        case None => inner(r :+ h, t, Some(h))
      }
    }

    inner(Nil, xs, None)
    //END
  }
}

object P09 {

  def pack[X](xs: List[X]): List[List[X]] = {
    //SOLUTION
    def addMaybe(xss: List[List[X]], xs: List[X]): List[List[X]] = xs match {
      case Nil => xss
      case w => xss :+ w
    }

    @scala.annotation.tailrec
    def inner(r: List[List[X]], s: List[X], w: List[X], xo: Option[X]): List[List[X]] = {
      w match {
        case Nil => addMaybe(r, s)
        case h :: t => xo match {
          case Some(x) =>
            h match {
              case `x` => inner(r, s :+ h, t, xo)
              case _ => inner(addMaybe(r, s), Nil, w, None)
            }
          case None => inner(r, s :+ h, t, Some(h))
        }
      }
    }

    inner(Nil, Nil, xs, None)
    //END
  }
}

object P10 {

  def encode[X](xs: List[X]): List[(Int, X)] = /*SOLUTION*/ for (zs <- P09.pack(xs)) yield zs.length -> zs.head /*END*/
}

object P11 {

  def encodeModified[X](xs: List[X]): List[Any] = /*SOLUTION*/ for ((l, x) <- P10.encode(xs)) yield if (l == 1) x else l -> x /*END*/
}

object P12 {

  def decode[X](xIs: List[(Int, X)]): List[X] = /*SOLUTION*/ P00.flatten(for ((l, x) <- xIs) yield P00.fill(l)(x)) /*END*/
}

object P13 {

  def encodeDirect[X](xs: List[X]): List[(Int, X)] = {
    //SOLUTION
    @scala.annotation.tailrec
    def inner(r: List[(Int, X)], xs: List[X], xo: Option[X], w: List[X]): List[(Int, X)] = {
      def terminate: List[(Int, X)] = xs match {
        case Nil => r
        case _ => r :+ xs.length -> xs.head
      }

      w match {
        case Nil => terminate
        case h :: t => xo match {
          case Some(x) => h match {
            case `x` => inner(r, xs :+ x, xo, t)
            case _ => inner(terminate, Nil, Some(h), w)
          }
          case None => inner(r, Nil, Some(h), w)
        }
      }
    }

    inner(Nil, Nil, None, xs)
    //END
  }
}

object P14 {

  def duplicate[X](xs: List[X]): List[X] = {
    //SOLUTION
    @scala.annotation.tailrec
    def inner(r: List[X], w: List[X]): List[X] = w match {
      case Nil => r
      case h :: t => inner(r :+ h :+ h, t)
    }

    inner(Nil, xs)
    //END
  }
}

object P15 {

  def duplicateN[X](n: Int, xs: List[X]): List[X] = {
    //SOLUTION
    @scala.annotation.tailrec
    def inner(r: List[X], w: List[X]): List[X] = w match {
      case Nil => r
      case h :: t => inner(r ++ P00.fill(n)(h), t)
    }

    inner(Nil, xs)
    //END
  }
}
