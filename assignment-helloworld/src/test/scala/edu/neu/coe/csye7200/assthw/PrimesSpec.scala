package edu.neu.coe.csye7200.assthw

import edu.neu.coe.csye7200.assthw.Primes.{EulerPrime, MaybePrime, eulerPrimes, primes}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PrimesSpec extends AnyFlatSpec with should.Matchers {

    behavior of "Primes"

    it should "isPrime" in {
        2L.isPrime shouldBe true
        3L.isPrime shouldBe true
        4L.isPrime shouldBe false
        5L.isPrime shouldBe true
        6L.isPrime shouldBe false
        561L.isPrime shouldBe false
        2147483647L.isPrime shouldBe true
    }

    ignore should "Mersenne 17 isPrime" in {
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
