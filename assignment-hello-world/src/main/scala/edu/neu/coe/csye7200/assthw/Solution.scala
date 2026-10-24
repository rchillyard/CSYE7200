package edu.neu.coe.csye7200.assthw

object Solution {

  def sumList(xs: List[Double]): Double = {
    def inner(total: Double, work: List[Double]): Double = work match {
      case Nil => total
      case x :: xs => inner(total + x, xs)
    }
    inner(0, xs)  
  }

  def main(args: Array[String]): Unit = {
    val nums: List[Double] = scala.io.StdIn.readLine().split(" ").map(_.toDouble).toList
    
    println(sumList(nums))
  }
}