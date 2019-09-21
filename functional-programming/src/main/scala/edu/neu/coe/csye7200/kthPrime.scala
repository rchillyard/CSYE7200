package edu.neu.coe.csye7200

object kthPrime extends App {

  def primeNumber(n: Int): Int = {
    @scala.annotation.tailrec
    def find(m: Int): Int = {
      if (isPrime(m)) m
      else find(m + 1)
    }

    find(n)
  }

  def isPrime(x: Int): Boolean = x match {
    case 2 | 3 => true
    case _ =>
      (2 to Math.sqrt(x).toInt).forall(y => x % y != 0)
  }

  println(primeNumber(10))

}
