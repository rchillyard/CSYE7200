package edu.neu.coe.csye7200.asstll

import edu.neu.coe.csye7200.asstll.FibonacciPythagoras._
import edu.neu.coe.csye7200.asstll.LazyListUtils.{iterateBFS, render}

/**
 * Please see https://youtu.be/94mV7Fmbx88?si=FJ1EYwG39jR-LuWq in order to understand what this is all about.
 */
object FibonacciPythagoras {
  val BigTwo: BigInt = BigInt(2)
  val BigOne: BigInt = BigInt(1)
  val BigZero: BigInt = BigInt(0)
  val root: FibSquare = FibSquare(1, 2)

  def square(x: BigInt): BigInt = x * x

  lazy val tree: LazyList[FibSquare] = iterateBFS(LazyList(root))(_.children)
  lazy val fermatTree: LazyList[FibSquare] = LazyList.iterate(root)(_.fermat)
  lazy val platoTree: LazyList[FibSquare] = LazyList.iterate(root)(_.plato)
  lazy val pythagorasTree: LazyList[FibSquare] = LazyList.iterate(root)(_.pythagoras)

  lazy val treePyTriple: LazyList[PyTriple] = tree map (_.triple)
  lazy val treeRational: LazyList[RationalPair] = tree map (_.pair)

}

trait Renderable {
  def render: String
}

case class FibSquare(u: BigInt, v: BigInt) extends Renderable {

  require(v.compare(u) > 0 && v.gcd(u) == BigOne && u.compare(BigZero) > 0 && u.mod(BigTwo) != v.mod(BigTwo), s"Invalid u, v: $u, $v")

  lazy val pre: BigInt = v - u
  lazy val post: BigInt = v + u

  lazy val triple: PyTriple = PyTriple(pre * post, 2 * u * v, pre * v + post * u)

  lazy val pair: RationalPair = RationalPair(Rational(pre, post), Rational(u, v))

  def child(m: Int, n: Int): FibSquare = FibSquare(u * (1 - m) + v * m, u * (2 * n - m) + v * (m + 1))

  lazy val children: LazyList[FibSquare] = plato #:: fermat #:: pythagoras #:: LazyList.empty

  lazy val fermat: FibSquare = child(1, 1)

  lazy val plato: FibSquare = child(0, 1)

  lazy val pythagoras: FibSquare = child(1, 0)

  lazy val fermatList: LazyList[FibSquare] = LazyList.iterate(this)(_.fermat)
  lazy val platoList: LazyList[FibSquare] = LazyList.iterate(this)(_.plato)
  lazy val pythagorasList: LazyList[FibSquare] = LazyList.iterate(this)(_.pythagoras)

  def render: String = s"[$pre,$u,$v,$post]"
}

object FibSquare {
  def apply(x: Int, y: Int): FibSquare = new FibSquare(BigInt(x), BigInt(y))
}

case class PyTriple(x: BigInt, y: BigInt, z: BigInt) extends Renderable {

  require(square(x) + square(y) == square(z), s"Not a Pythagorean triple: $x, $y, $z")

  def render: String = s"{$x,$y,$z}"
}

case class RationalPair(p: Rational, q: Rational) extends Renderable {
  def render: String = s"($p, $q)"
}

case class Rational(n: BigInt, d: BigInt) {
  def +(r: Rational): Rational = Rational.apply(n * r.d + r.n * d, d * r.d)

  def *(r: Rational): Rational = Rational.apply(n * r.n, d * r.d)

  override def toString: String = d match {
    case BigOne => n.toString
    case _ => s"$n/$d"
  }
}

object Rational {

  def apply(n: BigInt): Rational = apply(n, BigOne)

  def apply(n: BigInt, d: BigInt): Rational = {
    val f = n.gcd(d)
    new Rational(n / f, d / f)
  }
}

object LazyListUtils {

  def render[X <: Renderable](xs: LazyList[X], n: Int): Seq[String] = xs take n to List map (_.render)

  def iterateDFS[X](start: X)(f: X => LazyList[X]): LazyList[X] = {
    val head: X = start
    head #:: (for {x <- f(head); y <- iterateDFS(x)(f)} yield y)
  }

  def iterateBFS[X](start: LazyList[X])(f: X => LazyList[X]): LazyList[X] = {
    val xs: LazyList[X] = start flatMap f

    start #::: iterateBFS(xs)(f)
  }
}

object ShowTrees extends App {
  private def makeString(xs: Seq[String]): String = xs mkString ", "

  val length = 40

  println(s"Fibonacci Squares: ${makeString(render(tree, length))}")
  println(s"Pythagorean Triples: ${makeString(render(treePyTriple, length))}")
  println(s"Rational Pairs: ${makeString(render(treeRational, length))}")
}