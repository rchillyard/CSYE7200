/*
 * Copyright (c) 2019. Phasmid Software
 */

package edu.neu.coe.csye7200
  class S99Int(val start: Int) {
    import S99Int._

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
    def goldbach: (Int,Int) = ??? // TO BE IMPLEMENTED
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    // P31
    val primes = ??? // TO BE IMPLEMENTED

    // P32
    def gcd(m: Int, n: Int): Int = ??? // TO BE IMPLEMENTED

    // P39
    def listPrimesinRange(r: Range): Seq[Int] = ??? // TO BE IMPLEMENTED

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
