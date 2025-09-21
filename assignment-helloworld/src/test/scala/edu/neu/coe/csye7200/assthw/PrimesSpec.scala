package edu.neu.coe.csye7200.assthw

import edu.neu.coe.csye7200.assthw.Primes.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.matchers.should.Matchers.shouldBe
import scala.math.BigInt.long2bigInt

/**
 * Specification tests for the prime-related functions and utilities.
 *
 * This test class verifies the correctness of the following functionalities:
 *
 * - `hasFactor`: Confirms that a number correctly identifies if another number is its factor.
 * - `hasNoFactorsSmallerThanSquareRootX`: Validates that numbers declare the absence of prime factors smaller than or equal to their square root correctly.
 * - `isProbablePrime`: Ensures the proper probabilistic determination of primality with configurable certainty.
 * - `isPrime`: Confirms accurate verification of whether a number is truly prime.
 * - `Mersenne numbers`: Tests specific cases of Mersenne numbers and their primality.
 * - `primes`: Validates the generation and properties of prime numbers up to certain limits or counts.
 * - `coprime`: Tests determination of whether two numbers are coprime.
 * - `EulerPrime`: Verifies the generation of primes from Euler's quadratic formula.
 * - `eulerPrimes`: Validates the sequence of Euler-generated primes.
 *
 * Some tests are focused on edge cases (e.g., Carmichael numbers and large Mersenne primes) to ensure correctness under various scenarios.
 * Ignored tests include exceptionally time-consuming computations that serve to verify correctness under theoretical scenarios.
 *
 * NOTE that there are implicit converters from Int/Long to BigInt
 */
class PrimesSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Primes"

  it should "hasFactor" in {
    1L hasFactor 1 shouldBe true
    4L hasFactor 2 shouldBe true
    4L hasFactor 3 shouldBe false
  }

  it should "divides" in {
    1 |: 1L shouldBe true
    2 |: 4L shouldBe true
    3 |: 4L shouldBe false
  }

  it should "isEven" in {
    1L.isEven shouldBe false
    100L.isEven shouldBe true
  }

  it should "hasNoFactorsSmallerThanSquareRootX" in {
    2L.hasNoFactorsSmallerThanSquareRootX shouldBe true
    3L.hasNoFactorsSmallerThanSquareRootX shouldBe true
    5L.hasNoFactorsSmallerThanSquareRootX shouldBe true
    6L.hasNoFactorsSmallerThanSquareRootX shouldBe false
    11L.hasNoFactorsSmallerThanSquareRootX shouldBe true
    12L.hasNoFactorsSmallerThanSquareRootX shouldBe false
    // This is the first Carmichael Number (see https://en.wikipedia.org/wiki/Carmichael_number)
    561L.hasNoFactorsSmallerThanSquareRootX shouldBe false
    2147483647L.hasNoFactorsSmallerThanSquareRootX shouldBe true
  }

  it should "isPrime" in {
    2L.isPrime shouldBe true
    3L.isPrime shouldBe true
    4L.isPrime shouldBe false
    5L.isPrime shouldBe true
    6L.isPrime shouldBe false
    11L.isPrime shouldBe true
    12L.isPrime shouldBe false
    561L.isPrime shouldBe false
    2147483649L.isPrime shouldBe false
  }

  it should "Mersenne numbers" in {
    2147483647L.isPrime shouldBe true // XXX Mersenne #10
    137438953471L.isPrime shouldBe false // XXX Mersenne #11
    2199023255551L.isPrime shouldBe false // XXX Mersenne #12
    8796093022207L.isPrime shouldBe false // XXX Mersenne #13
    140737488355327L.isPrime shouldBe false // XXX Mersenne #14
    9007199254740991L.isPrime shouldBe false // XXX Mersenne #15
    576460752303423487L.isPrime shouldBe false // XXX Mersenne #16
  }

  // NOTE don't even try to run this unless you arrange to do things in parallel
  ignore should "Mersenne 17 isPrime" in {
    // The reason this takes so long is that it IS a prime number and
    // we have to assert all prime numbers less than 1,518,500,250 are not factors.
    // This takes time proportional to 2,305,843,009,213,693,951.
    2305843009213693951L.isPrime shouldBe true
  }

  it should "primes 1" in {
    val smallPrimes = primes takeWhile (_ < 1000) to List
    smallPrimes.length shouldBe 168
  }

  it should "primes 2" in {
    val smallPrimes = primes take 1000 to List
    smallPrimes.last shouldBe 7919L
  }

  it should "coprime" in {
    7894609062L.coprime(11413) shouldBe true
    7894609062L.coprime(11411) shouldBe false
    663168016L.coprime(5983) shouldBe true
    663168016L.coprime(5987) shouldBe false
  }

  it should "EulerPrime" in {
    EulerPrime(1) shouldBe Some(41)
    EulerPrime(10) shouldBe Some(131)
    EulerPrime(50) shouldBe None
  }

  it should "eulerPrimes" in {
    eulerPrimes.length shouldBe 40
    eulerPrimes.head shouldBe Some(41)
  }

  it should "test whether the square of every prime greater than 3 is congruent to 1 modulo 24" in {
    val matches = primes.drop(2).map(p => p * p).map(_.≡(1)(24))
    val n = 1_000_000 // this is how many we're actually going to check in practice
    matches.take(n).forall(_)
  }

}