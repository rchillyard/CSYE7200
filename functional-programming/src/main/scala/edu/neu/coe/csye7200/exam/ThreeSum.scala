package edu.neu.coe.csye7200.exam

case class ThreeSum(xs: Seq[Int]) {

  def triples: Seq[(Int,Int,Int)] = {
    val z: Seq[(Int, Int)] = xs.zipWithIndex
    ???
  }
}

object ThreeSum extends App {
  val xs = Seq(1, 4, -2, -3, 5, 0, -1, 7, -4, 2)
  val q = ThreeSum(xs).triples
  if (q.size==5)
    println(q)
  else
    println(s"wrong size: ${q.size}")
}
