package edu.neu.coe.csye7200.fp.reduction

import scala.annotation.tailrec

/**
  * This is from a problem in LeetCode: 780 Reaching Points (Hard)
  */
trait Moves {
  /**
    * Method to determine if we can move successfully from point x
    *
    * @param x the point
    * @return true if there is such a path
    */
  def valid(x: Point): Boolean

  /**
    * This method defines the possible moves from point p
    *
    * @param p     the point
    * @param which the strategy to use
    * @return the point we moved to
    */
  def move(p: Point, which: Boolean): Point
}

case class Point(x: Int, y: Int) {

  /**
    * Test whether s is aligned with this point.
    *
    * @param s the point to be compared with this point.
    * @return a tuple of Boolean (aligned) and the perpendicular coordinate of s
    */
  def alignment(s: Point): (Boolean, Int) = if (x == s.x) (true, s.y) else if (y == s.y) (true, s.x) else (false, 0)

  /**
    * If this point is on final approach to s, return true.
    * Final approach is define as vertically or horizontally aligned with s and at the correct distance from s.
    *
    * @param s the point we want to approach
    * @return true if the conditions are (effectively) successful
    */
  def onFinalApproach(s: Point): Boolean = {
    val (ok, x) = alignment(s)
    ok && distance(s) % x == 0
  }


  override def toString: String = "Point{" + "x=" + x + ", y=" + y + '}'

  /**
    * Gets the non-Euclidean distance (we can't go in a straight line so this is proper) from this point to the target
    *
    * @param target our desired destination
    * @return the total number of steps we must take to get to target
    */
  def distance(target: Point): Int = target.x - x + target.y - y

  def valid: Boolean = x > 0 && y > 0
}

/**
  *
  * @param t The target point that we wish to reach
  */
case class Moves1(t: Point) extends Moves {

  def valid(p: Point): Boolean = inner(p :: Nil, result = false)

  @tailrec private def inner(points: List[Point], result: Boolean): Boolean = points match {
    case Nil => result
    case x :: remainder => x match {
      case `t` => true
      case _ =>
        if (x.x > t.x || x.y > t.y) inner(remainder, result = false)
        else {
          val addToY = move(x, which = true)
          val addToX = move(x, which = false)
          val work =
            if (addToY.distance(t) < addToX.distance(t))
              addToY +: remainder :+ addToX
            else
              addToX +: remainder :+ addToY
          inner(work, result)
        }
    }
  }

  override def move(p: Point, which: Boolean): Point = if (which) Point(p.x, p.x + p.y) else Point(p.x + p.y, p.y)
}

object Moves1 {
  def apply(x: Int, y: Int): Moves1 = apply(Point(x, y))
}

/**
  * In Moves2, we work backwards from the target to the start.
  *
  * This is not sufficiently fast to satisfy the LeetCode challenge.
  *
  * @param s the start point that we wish to reach
  */
case class Moves2(s: Point) extends Moves {

  def valid(t: Point): Boolean = {
    @tailrec
    def inner(p: Point): Boolean = if (p == s) true else if (!p.valid) false else inner(move(p, which = true))

    inner(t)
  }

  /**
    * This method defines the possible moves from point p
    *
    * @param p     the point
    * @param which ignored
    * @return the point we moved to
    */
  override def move(p: Point, which: Boolean): Point = if (p.y > p.x) Point(p.x, p.y - p.x) else Point(p.x - p.y, p.y)
}

object Moves2 {
  def apply(x: Int, y: Int): Moves2 = apply(Point(x, y))
}

/**
  * In Moves3, we work backwards from the target to the start, like in Moves2.
  * But we add another optimization.
  *
  * @param s the start point that we wish to reach
  */
case class Moves3(s: Point) extends Moves {

  def valid(t: Point): Boolean = {
    @tailrec
    def inner(p: Point): Boolean = if (p.onFinalApproach(s)) true else if (p == s) true else if (!p.valid) false else inner(move(p, which = true))

    inner(t)
  }

  /**
    * This method defines the possible moves from point p
    *
    * @param p     the point
    * @param which ignored
    * @return the point we moved to
    */
  override def move(p: Point, which: Boolean): Point = if (p.y > p.x) Point(p.x, p.y - p.x) else Point(p.x - p.y, p.y)
}

object Moves3 extends App {
  def apply(x: Int, y: Int): Moves3 = apply(Point(x, y))
}
