package edu.neu.coe.csye7200.assthw

import edu.neu.coe.csye7200.assthw.Primes.{EulerPrime, MaybePrime, eulerPrimes, primes}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PrimesSpec extends AnyFlatSpec with should.Matchers {

    behavior of "Primes"

    it should "hasNoFactorsSmallerThanRoot" in {
        2L.hasNoFactorsSmallerThanRoot shouldBe true
        3L.hasNoFactorsSmallerThanRoot shouldBe true
        5L.hasNoFactorsSmallerThanRoot shouldBe true
        6L.hasNoFactorsSmallerThanRoot shouldBe false
        11L.hasNoFactorsSmallerThanRoot shouldBe true
        12L.hasNoFactorsSmallerThanRoot shouldBe false
        561L.hasNoFactorsSmallerThanRoot shouldBe false
        2147483647L.hasNoFactorsSmallerThanRoot shouldBe true
    }

    it should "isProbablePrime" in {
        2L.isProbablePrime(20) shouldBe true
        3L.isProbablePrime(20) shouldBe true
        5L.isProbablePrime(20) shouldBe true
        6L.isProbablePrime(20) shouldBe false
        11L.isProbablePrime(20) shouldBe true
        12L.isProbablePrime(20) shouldBe false
        561L.isProbablePrime(20) shouldBe false
        2147483647L.isProbablePrime(20) shouldBe true
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

}