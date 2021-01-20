package edu.neu.coe.csye7200.fp.exam

trait Triples[X] {

  def triples(xs: Seq[X])(p: (X, X, X) => Boolean): Seq[(X, X, X)] = {
    val z: Seq[(X, Int)] = xs.zipWithIndex
    for ((x_i, i) <- z; (x_j, j) <- z; if j > i; (x_k, k) <- z; if k > j; if p(x_i, x_j, x_k)) yield (x_i, x_j, x_j)
  }

}

abstract class NumberTriples[X: Numeric] extends Triples[X] {

  private val nx = implicitly[Numeric[X]]

  def triplesSum(xs: Seq[X])(t: X): Seq[(X, X, X)] = triples(xs)((x1, x2, x3) => nx.plus(nx.plus(x1, nx.plus(x2, x3)), nx.negate(t)) == nx.zero)

}

case class ThreeSum(xs: Seq[Int]) extends NumberTriples[Int] {

  def triplesZeroSum: Seq[(Int, Int, Int)] = triplesSum(xs)(0)
}

object ThreeSum extends App {
  val xs = Seq(1, 4, -2, -3, 5, 0, -1, 7, -4, 2)
  val q = ThreeSum(xs).triplesZeroSum
  if (q.size == 8)
    println(q)
  else
    println(s"wrong size: ${q.size}")
}
