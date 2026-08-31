package edu.neu.coe.csye7200.assthw

import scala.language.implicitConversions

/**
 * Provides functionality related to prime numbers and their associated operations,
 * including utilities for checking primality, generating primes, and arithmetic on `BigInt`.
 */
object Primes {

  /**
   * Extension methods for the BigInt type to provide various utility functions related to primes and factors.
   */
  extension (x: BigInt) {

    /**
     * Determines if the BigInt `that` is a factor of the current BigInt `x`.
     *
     * @param that the potential factor to check, represented as a BigInt.
     * @return true if `that` is a factor of `x`, otherwise false.
     */
    def hasFactor(that: BigInt): Boolean =
    // TO BE IMPLEMENTED 
        ???

    /**
     * Determines if the given `BigInt` `that` divides the current `BigInt` `x` without a remainder.
     *
     * @param that the divisor to check, represented as a `BigInt`.
     * @return true if `that` divides `x` without a remainder, otherwise false.
     */
    def divides(that: BigInt): Boolean = that hasFactor x

    /**
     * Identical to `divides`.
     *
     * @param other the potential other to check, represented as a `BigInt`.
     * @return true if the given `BigInt` other is a other of the current `BigInt`, otherwise false.
     */
    def |:(other: BigInt): Boolean = divides(other)

    /**
     * Determines if the current `BigInt` and another `BigInt` are coprime.
     * Two numbers are considered coprime if their greatest common divisor (GCD) is 1.
     *
     * @param that the `BigInt` to check coprimality with the current `BigInt`.
     * @return true if the two `BigInt` values are coprime, otherwise false.
     */
    def coprime(that: BigInt): Boolean =
    // TO BE IMPLEMENTED 
        ???

    /**
     * Checks whether `x`, the current `BigInt`, is congruent to `that`, another `BigInt`, modulo a specified modulus (`mod`).
     * Two numbers `x` and `that` are congruent modulo `mod` if `(x - that) % mod == 0`.
     *
     * @param that the `BigInt` to check the congruence with.
     * @param mod  the modulus for checking congruence, represented as a `BigInt`.
     * @return true if the current `BigInt` is congruent to `that` modulo `mod`, otherwise false.
     */
    def isCongruent(that: BigInt)(mod: BigInt): Boolean =
      (x - that) hasFactor mod

    /**
     * Identical to `isCongruent`.
     *
     * @param that the `BigInt` to check the congruence with.
     * @param mod  the modulus for checking congruence, represented as a `BigInt`.
     * @return true if the current `BigInt` is congruent to `that` modulo `mod`, otherwise false.
     */
    def ≡(that: BigInt)(mod: BigInt): Boolean =
      isCongruent(that)(mod)

    /**
     * Computes the modular exponentiation of a `BigInt`.
     * Calculates `(x^exp) mod m`, where `x` is the current `BigInt` instance.
     *
     * @param exp the exponent to which `x` is raised, represented as a `BigInt`.
     * @param m   the modulus under which the calculation is performed, represented as a `BigInt`.
     * @return the result of `(x^exp) mod m`, as a `BigInt`.
     */
    def modPow(exp: BigInt, m: BigInt): BigInt = x modPow(exp, m)

    /**
     * Determines if the current `BigInt` is an even number.
     *
     * @return true if the `BigInt` is even, false otherwise.
     */
    def isEven: Boolean = !x.testBit(0)

    /**
     * Method to determine if `x` is actually prime.
     * First, we test if `x` is 2. If it is, then it is prime.
     * Otherwise, we test that `x` is not even. If it is even, then it is not prime.
     * Otherwise, we test whether `isProbablePrime` is true (with a certainty of 20). If it is not, then it is not prime.
     * Otherwise, we check that no prime numbers smaller than sqrt(x) are factors.
     *
     * @return true if x is indeed prime, false otherwise.
     */
    def isPrime: Boolean =
    // TO BE IMPLEMENTED 
        ???

    /**
     * Determines if there are no prime factors of `x` smaller than or equal to the square root of `x`.
     * It filters the list of primes up to the square root of `x`, then checks whether `x` has any of these as factors.
     *
     * @return true if there are no factors of `x` among primes smaller than or equal to the square root of `x`, otherwise false.
     */
    def hasNoFactorsSmallerThanSquareRootX: Boolean =
      (primes takeWhile (y => y * y <= x)) forall (y => !x.hasFactor(y))
  }

  import LazyList.from
  import scala.Option.when

  /**
   * Calculate the value of a prime number based on the formula n * n - n + 41 where n > 0.
   * If the result is prime then return it wrapped in Some. Otherwise, return None.
   *
   * @param n a positive Int.
   * @return Some(prime) otherwise None.
   */
  def EulerPrime(n: Int): Option[BigInt] =
  // TO BE IMPLEMENTED 
    ???

  /**
   * Create an infinitely long lazy list of BigInts, that are prime numbers.
   * All primes are odd, except for the very first prime number (2).
   */
  lazy val primes: LazyList[BigInt] =
  // TO BE IMPLEMENTED 
    ???

  /**
   * Create a finite list of Option[BigInt], such that each element is the (successful) result of invoking EulerPrime
   * on the current Int.
   * Start with a lazy list of Ints beginning with 1.
   * Do not include any results that are empty.
   */
  lazy val eulerPrimes: List[Option[BigInt]] =
  // TO BE IMPLEMENTED 
    ???
}
