package edu.neu.coe.csye7200.prime

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PrimeSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Prime"

  it should "isPrime" in {
    val p2 = Prime(2)
    p2.isPrime shouldBe true
    p2.isPrime shouldBe true
    Prime(3).isPrime shouldBe true
    Prime(4).isPrime shouldBe false
    Prime(5).isPrime shouldBe true
    Prime(6).isPrime shouldBe false
    Prime(7).isPrime shouldBe true
  }
  behavior of "Prime$"

  it should "primes 1" in {
    Prime.primes take 4 shouldBe List(Prime(2), Prime(3), Prime(5), Prime(7))
    Prime.primes map (_.x.toInt) take 100 shouldBe LazyList(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541)
  }

  it should "primes 2" in {
    Prime.primes.takeWhile(_.x < 524288).size shouldBe 43390
  }
  it should "primes 3" in {
    Prime.primes.takeWhile(_.x < 524288).size shouldBe 43390
  }

  it should "create Mersenne primes 1" in {
    Prime.createMersennePrime(0).isPrime shouldBe true
    Prime.createMersennePrime(3).isPrime shouldBe true
    Prime.createMersennePrime(4).isPrime shouldBe false
    Prime.createMersennePrime(7).isPrime shouldBe true
  }

  it should "create Mersenne primes 2" in {
    Prime.createMersennePrime(8).isPrime shouldBe false
    Prime.createMersennePrime(9).isPrime shouldBe false
    Prime.createMersennePrime(10).isPrime shouldBe true
    Prime.createMersennePrime(11).isPrime shouldBe false
    Prime.createMersennePrime(12).isPrime shouldBe false
    Prime.createMersennePrime(13).isPrime shouldBe false
    Prime.createMersennePrime(14).isPrime shouldBe false
    Prime.createMersennePrime(15).isPrime shouldBe false
    Prime.createMersennePrime(16).isPrime shouldBe false
  }

  // NOTE: This is very slow!!!! Got only as far as 352,292,719 in about 30 minutes
  // However, we would need to get up to 1,518,500,250 to test this
  ignore should "create Mersenne primes 3" in {
    // In order to evaluate this prime, we must evaluate all primes up to 2^30.5.
    Prime.createMersennePrime(17).isPrime shouldBe true
  }

  it should "apply" in {
    Prime(2) shouldBe new Prime(2, None)
  }

  it should "probablePrimes 1" in {
    Prime.probablePrimes take 2 shouldBe List(Prime(2), Prime(3))
    Prime.probablePrimes take 3 shouldBe List(Prime(2), Prime(3), Prime(5))
  }
  it should "probablePrimes 2" in {
    Prime.probablePrimes take 3 shouldBe List(Prime(2), Prime(3), Prime(5))
    Prime.probablePrimes.takeWhile(_.x < 524288).size shouldBe 43390
  }

}