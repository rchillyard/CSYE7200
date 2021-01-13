/*
 * Copyright (c) 2019. Phasmid Software
 */

package edu.neu.coe.csye7200.scala99

import scala.language.implicitConversions

class Arithmetic(val start: Int) {

  import Arithmetic._

    // P31
    def isPrime: Boolean = (start > 1) &&
      primes.takeWhile(_ <= Math.sqrt(start)).forall(start % _ != 0)

  // P33
  def isCoprimeTo(n: Int): Boolean = /*SOLUTION*/ gcd(start, n) == 1 /*END*/

  // P34
  def totientP34: Int = /*SOLUTION*/ (1 to start).count(isCoprimeTo) /*END*/

    // P37
    def totient: Int = /*SOLUTION*/ primeFactorMultiplicity.foldLeft(1) { (r, f) =>
      f match { case (p, m) => r * (p - 1) * Math.pow(p, m - 1).toInt }
    } /*END*/

  // P35 (amended by P36)
  def primeFactors: Seq[Int] = /*SOLUTION*/ for ((k, v) <- primeFactorMultiplicity.toSeq; z <- P00.fill(v)(k)) yield z /*END*/

  // P36
    def primeFactorMultiplicity: Map[Int,Int] = {
      //SOLUTION
      def factorCount(n: Int, p: Int): (Int, Int) =
        if (n % p != 0) (0, n)
        else factorCount(n / p, p) match { case (c, d) => (c + 1, d) }

      def factorsR(n: Int, ps: LazyList[Int]): Map[Int, Int] =
        if (n == 1) Map()
        else if (n.isPrime) Map(n -> 1)
        else {
          val nps = ps.dropWhile(n % _ != 0)
          val (count, dividend) = factorCount(n, nps.head)
          Map(nps.head -> count) ++ factorsR(dividend, nps.tail)
        }
      factorsR(start, primes)
      //END
    }

  // P40
  def goldbach: (Int, Int) = /*SOLUTION*/
    primes takeWhile { _ < start } find { p => (start - p).isPrime } match {
      case None => throw new IllegalArgumentException
      case Some(p1) => (p1, start - p1)
    } /*END*/
}

object Arithmetic {
  implicit def int2S99Int(i: Int): Arithmetic = new Arithmetic(i)

  // P31
  lazy val primes: LazyList[Int] = 2 #:: LazyList.from(3, 2).filter(_.isPrime)

  // P32
  @scala.annotation.tailrec
  def gcd(m: Int, n: Int): Int = /*SOLUTION*/ if (n == 0) m else gcd(n, m % n) /*END*/

    // P39
    def listPrimesInRange(r: Range): Seq[Int] = /*SOLUTION*/
      primes dropWhile { _ < r.start } takeWhile { _ <= r.last }/*END*/

  // P41
    def printGoldbachList(r: Range): Unit = {
      //SOLUTION
      printGoldbachListLimited(r, 0)
      //END
    }

    // P41
    def printGoldbachListLimited(r: Range, limit: Int): Unit = {
      //SOLUTION
      (r filter { n => n > 2 && n % 2 == 0 } map { n => (n, n.goldbach) }
        filter {_._2._1 >= limit} foreach { case (n, (p1, p2)) => println(s"$n = $p1 + $p2") })
      //END
    }

  }
