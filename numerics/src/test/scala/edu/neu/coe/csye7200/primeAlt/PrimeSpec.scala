package edu.neu.coe.csye7200.primeAlt

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PrimeSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Prime"

  it should "isDefinitePrime" in {
    val p2 = new Prime(2)
    p2.isDefinitePrime shouldBe true
    p2.isDefinitePrime shouldBe true
    new Prime(3).isDefinitePrime shouldBe true
    Prime(4).isDefinitePrime shouldBe false
    new Prime(5).isDefinitePrime shouldBe true
    Prime(6).isDefinitePrime shouldBe false
    new Prime(7).isDefinitePrime shouldBe true
  }

  behavior of "Prime$"

  ignore should "primes 1" in {
    Prime.primes take 4 shouldBe List(Prime(2), Prime(3), Prime(5), Prime(7))
    Prime.primes map (_.x.toInt) take 100 shouldBe LazyList(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541)
  }

  ignore should "primes 2" in {
    Prime.primes.takeWhile(_.x < 524288).size shouldBe 43390
  }
  ignore should "primes 3" in {
    Prime.primes.takeWhile(_.x < 524288).size shouldBe 43390
  }

  ignore should "create Mersenne primes 1" in {
    Prime.createMersennePrime(0) should matchPattern { case _: Prime => }
    Prime.createMersennePrime(3) should matchPattern { case _: Prime => }
    Prime.createMersennePrime(4) should matchPattern { case _: Composite => }
    Prime.createMersennePrime(7) should matchPattern { case _: Prime => }
  }

  ignore should "create Mersenne primes 2" in {
    Prime.createMersennePrime(8) should matchPattern { case _: Composite => }
    Prime.createMersennePrime(9) should matchPattern { case _: Composite => }
    Prime.createMersennePrime(10) should matchPattern { case _: Prime => }
    Prime.createMersennePrime(11) should matchPattern { case _: Composite => }
    Prime.createMersennePrime(12) should matchPattern { case _: Composite => }
    Prime.createMersennePrime(13) should matchPattern { case _: Composite => }
    Prime.createMersennePrime(14) should matchPattern { case _: Composite => }
    Prime.createMersennePrime(15) should matchPattern { case _: Composite => }
    Prime.createMersennePrime(16) should matchPattern { case _: Composite => }
  }

  // NOTE: This is very slow!!!! Got only as far as 352,292,719 in about 30 minutes
  // However, we would need to get up to 1,518,500,250 to test this
  ignore should "create Mersenne primes 3" in {
    // In order to evaluate this prime, we must evaluate all primes up to 2^30.5.
    Prime.createMersennePrime(17) should matchPattern { case _: Prime => }
  }

  ignore should "apply" in {
    Prime(2) shouldBe new Prime(2)
  }

  ignore should "possiblePrimes 1" in {
    Prime.possiblePrimes take 2 shouldBe List(Prime(2), Prime(3))
    Prime.possiblePrimes take 3 shouldBe List(Prime(2), Prime(3), Prime(5))
  }
  ignore should "possiblePrimes 2" in {
    Prime.possiblePrimes take 3 shouldBe List(Prime(2), Prime(3), Prime(5))
    Prime.possiblePrimes.takeWhile(_.x < 524288).size shouldBe 43390
  }

}