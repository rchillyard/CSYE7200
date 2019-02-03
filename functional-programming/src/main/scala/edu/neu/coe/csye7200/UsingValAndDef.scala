package edu.neu.coe.csye7200

/**
  * Created by scalaprof on 2/19/17.
  */
object UsingValAndDef extends App {

  val sumOfFirst20Integers = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17 + 18 + 19 + 20

  def sumOfNIntegers(n: Int) = n * (n + 1) / 2

  val x = List(1, 2, 3).map(_ * 2)
  println(x)
}
