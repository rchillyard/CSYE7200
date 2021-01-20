package edu.neu.coe.csye7200

import scala.annotation.tailrec

trait IList {
  def x0: Boolean = this match {case INil => true; case _ => false }

  def x1: Int = this match {
    case INil => 0
    case Cons(_, tl) => 1 + tl.x1
  }

  def x2: Int = this match {
    case INil => 0
    case Cons(hd, tl) => hd + tl.x2
  }

  def x2a: Int = this match {
    case INil => throw new Exception("logic error");
    case Cons(hd, _) => hd
  }

  def x3: IList = this match {
    case INil => INil;
    case Cons(_, tl) => tl
  }

  def x3a: Option[Int] = this match {
    case INil => None;
    case Cons(hd, _) => Some(hd)
  }

  def x4(x: Int): Option[Int] = {
    @tailrec def inner(as: IList, x: Int): Option[Int] = as match {
      case INil => None
      case Cons(hd, tl) => if (x == 0) Some(hd) else inner(tl, x - 1)
    }

    if (x < 0) None else inner(this, x)
  }

}

case object INil extends IList

case class Cons(head: Int, tail: IList) extends IList
