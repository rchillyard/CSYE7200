/*
 * Copyright (c) 2019. Phasmid Software
 */

package edu.neu.coe.csye7200

class Arithmetic(val start: Int) {

  import Arithmetic._

    // P31
    def isPrime: Boolean = ??? // TO BE IMPLEMENTED

  // P33
  def isCoprimeTo(n: Int): Boolean = ??? // TO BE IMPLEMENTED

  // P34
  def totientP34: Int = ??? // TO BE IMPLEMENTED

    // P37
    def totient: Int = ??? // TO BE IMPLEMENTED

  // P35 (amended by P36)
  def primeFactors: Seq[Int] = ??? // TO BE IMPLEMENTED

  // P36
    def primeFactorMultiplicity: Map[Int,Int] = {
      // TO BE IMPLEMENTED
      ???
    }

  // P40
  def goldbach: (Int, Int) = ??? // TO BE IMPLEMENTED
}

object Arithmetic {
  implicit def int2S99Int(i: Int): Arithmetic = new Arithmetic(i)

  // P31
  val primes: LazyList[Int] = ??? // TO BE IMPLEMENTED

  // P32
  @scala.annotation.tailrec
  def gcd(m: Int, n: Int): Int = ??? // TO BE IMPLEMENTED

    // P39
    def listPrimesInRange(r: Range): Seq[Int] = ??? // TO BE IMPLEMENTED

  // P41
    def printGoldbachList(r: Range) {
      // TO BE IMPLEMENTED
      ???
    }

    // P41
    def printGoldbachListLimited(r: Range, limit: Int) {
      // TO BE IMPLEMENTED
      ???
    }

  }
