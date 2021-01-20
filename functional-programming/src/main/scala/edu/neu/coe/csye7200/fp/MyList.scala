package edu.neu.coe.csye7200

import scala.annotation.tailrec

trait MyList[+A] {
  override def toString: String = {
    @tailrec def tos(as: MyList[A], r: StringBuffer): CharSequence = as match {
      case MyNil => r
      case MyCons(hd, tl) => tos(tl, r.append((if (r.length > 1) ", " else "") + s""""$hd""""))
    }

    tos(this, new StringBuffer("(")) + ")"
  }

  def ++[B >: A](x: MyList[B]): MyList[B] = this match {
    case MyNil => x
    case MyCons(hd, tl) => MyCons(hd, tl ++ x)
  }

  def length: Int = {
    @tailrec def len(as: MyList[A], x: Int): Int = as match {
      case MyNil => x
      case MyCons(_, tl) => len(tl, x + 1)
    }

    len(this, 0)
  }

  def isEmpty: Boolean = this match {
    case MyNil => true;
    case _ => false
  }

  def x0: Boolean = this match {
    case MyNil => true;
    case _ => false
  }

  def x1: Int = this match {
    case MyNil => 0
    case MyCons(_, tl) => 1 + tl.x1
  }

  //  def x2: A = this match {
  //    case INil$ => unit(0)
  //    case MyCons(hd, tl) => hd + tl.x2
  //  }

  def x2a: A = this match {
    case MyNil => throw new Exception("logic error");
    case MyCons(hd, _) => hd
  }

  def x3: MyList[A] = this match {
    case MyNil => MyNil;
    case MyCons(_, tl) => tl
  }

  def x3a: Option[A] = this match {
    case MyNil => None;
    case MyCons(hd, _) => Some(hd)
  }

  def x4(x: Int): Option[A] = {
    @tailrec def inner(as: MyList[A], x: Int): Option[A] = as match {
      case MyNil => None
      case MyCons(hd, tl) => if (x == 0) Some(hd) else inner(tl, x - 1)
    }

    if (x < 0) None else inner(this, x)
  }


  def apply(idx: Int): A = {
    @tailrec def internal(as: MyList[A], x: Int): A = as match {
      case MyNil => throw new IndexOutOfBoundsException(s"index out of bounds: $idx")
      case MyCons(hd, tl) => if (x == 0) hd else internal(tl, x - 1)
    }

    if (idx >= 0) internal(this, idx) else throw new IndexOutOfBoundsException(s"index out of bounds: $idx")
  }

  def filter(f: A => Boolean): MyList[A] = this match {
    case MyCons(hd, tl) => val ftl = tl.filter(f); if (f(hd)) MyCons(hd, ftl) else ftl
    case MyNil => MyNil
  }

  def find(f: A => Boolean): Option[A] = this match {
    case MyCons(hd, tl) => if (f(hd)) Some(hd) else tl.find(f)
    case MyNil => None
  }

  def map[B](f: A => B): MyList[B] = this match {
    case MyCons(hd, tl) => MyCons(f(hd), tl.map(f));
    case MyNil => MyList[B]()
  }

  def flatMap[B](f: A => MyList[B]): MyList[B] = this match {
    case MyCons(hd, tl) => f(hd) ++ tl.flatMap(f)
    case MyNil => MyList[B]()
  }

  def foreach(f: A => Unit): Unit = this match {
    case MyCons(hd, tl) => f(hd); tl.foreach(f)
    case MyNil =>
  }

  def count(f: Counter[A]): MyList[Int] = this map f

  def foldLeft[B](z: B)(f: (B, A) => B): B = this match {
    case MyNil => z
    case MyCons(hd, tl) => tl.foldLeft(f(z, hd))(f)
  }

  //  def sum[B]: B = foldLeft(B.unit)(B.plus(_,_))
}

case object MyNil extends MyList[Nothing]

case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A] {
  def equals[B >: A](z: MyList[B]): Boolean = tail match {
    case MyNil => false
    case MyCons(x, xs) => z match {
      case MyCons(y, ys) => x == y && xs == ys
    }
  }
}

abstract class Counter[-A] extends (A => Int)

object MyList {
  type IntList = MyList[Int]

  def sum(ints: IntList): Int = ints match {
    case MyNil => 0
    case MyCons(x, xs) => x + sum(xs)
  }

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) MyNil
    else MyCons(as.head, apply(as.tail: _*))
}

//trait Functor[F[_]] {
//  def map[A,B](fa: F[A])(f: A => B): F[B]
//}
