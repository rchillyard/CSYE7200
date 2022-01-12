/*
 * Copyright (c) 2019. Phasmid Software
 */

package edu.neu.coe.csye7200.lab99.scala99

import scala.language.implicitConversions

class Arithmetic(val start: Int) {

  import Arithmetic._

  // P31
  def isPrime: Boolean = (start > 1) &&
          primes.takeWhile(_ <= Math.sqrt(start)).forall(start % _ != 0)

  // P33
  def isCoprimeTo(n: Int): Boolean = ??? // TO BE IMPLEMENTED

  // P34
  def totientP34: Int = ??? // TO BE IMPLEMENTED

  // P37
  def totient: Int = ??? // TO BE IMPLEMENTED

  // P35 (amended by P36)
  def primeFactors: Seq[Int] = ??? // TO BE IMPLEMENTED

  // P36
  def primeFactorMultiplicity: Map[Int, Int] = {
    // TO BE IMPLEMENTED
    ???
  }

  // P40
  def goldbach: (Int, Int) = ??? // TO BE IMPLEMENTED
}

object Arithmetic {
  implicit def int2S99Int(i: Int): Arithmetic = new Arithmetic(i)

  // P31
  lazy val primes: LazyList[Int] = 2 #:: LazyList.from(3, 2).filter(_.isPrime)

  // P32
//  @scala.annotation.tailrec
  def gcd(m: Int, n: Int): Int = ??? // TO BE IMPLEMENTED

  // P39
  def listPrimesInRange(r: Range): Seq[Int] = ??? // TO BE IMPLEMENTED

  // P41
  def printGoldbachList(r: Range): Unit = {
    // TO BE IMPLEMENTED
    ???
  }

  // P41
  def printGoldbachListLimited(r: Range, limit: Int): Unit = {
    // TO BE IMPLEMENTED
    ???
  }

}
