package edu.neu.coe.csye7200.primeAlt

import com.typesafe.scalalogging.Logger
import edu.neu.coe.csye7200.prime.Prime.{logger, primes}
import scala.math.ScalaNumber

abstract class BigIntLike(val x: BigInt) extends ScalaNumber {
  def isDefinitePrime: Boolean

  def toPrime: BigIntLike

  def probablyPrime: Boolean

  def isWhole: Boolean = true

  def underlying(): AnyRef = x

  def modPow(exp: BigInt, m: BigInt): BigInt = x.modPow(exp, m)

  def isProbablePrime(certainty: Int): Boolean = x.isProbablePrime(certainty)

  def intValue(): Int = x.intValue

  def longValue(): Long = x.longValue

  def floatValue(): Float = x.floatValue

  def doubleValue(): Double = x.doubleValue

  override def equals(obj: Any): Boolean = obj.getClass == classOf[BigIntLike] && x == obj.asInstanceOf[BigIntLike].x

  override def hashCode(): Int = x.hashCode()

  /**
   * Mutating method to determine if this Prime is truly prime.
   * As a side-effect, <code>valid</code> will be set to Some(true) if truly prime, otherwise, Some(false)
   *
   * @return a Boolean.
   */
  protected def validate: Boolean = {

    def composite = primes.takeWhile(p => p.x * p.x <= x).exists(p => x % p.x == 0)

    val isPrime = x == BigInt(2) || x == BigInt(3) || !composite
    logger.whenDebugEnabled(if (isPrime) logger.debug(s"$x is prime"))
    isPrime
  }
}

/**
 * This case class is reserved for prime numbers which are definitely prime, not just probably prime.
 * NOTE do not use the new constructor for this class, use Prime.apply instead.
 *
 * @param value a prime number as a BigInt.
 */
case class Prime(value: BigInt) extends BigIntLike(value) {

  def isDefinitePrime: Boolean = true

  def toPrime: BigIntLike = this

  def probablyPrime: Boolean = true
}

/**
 * This case class is reserved for composite (non-prime) numbers.
 *
 * @param value a BigInt.
 */
case class Composite(value: BigInt) extends BigIntLike(value) {

  def isDefinitePrime: Boolean = true

  def toPrime: BigIntLike = this

  def probablyPrime: Boolean = true
}

/**
 * This class is a wrapper for a BigInt where we do not know for sure if the value is prime.
 *
 * @param value a BigInt.
 */
case class MaybePrime(value: BigInt) extends BigIntLike(value) {

  lazy val probablyPrime: Boolean = isProbablePrime(20)

  lazy val isDefinitePrime: Boolean = probablyPrime && validate

  def toPrime: BigIntLike = if (probablyPrime && validate) Prime(value) else Composite(value)
}

object Prime {

  val logger: Logger = com.typesafe.scalalogging.Logger(classOf[Prime])
  val possiblePrimes: LazyList[BigIntLike] = (BigInt(2L) #:: LazyList.iterate(BigInt(3))(b => b + 2)) filter (_.isProbablePrime(20)) map apply
  val primes: LazyList[Prime] = possiblePrimes filter { p =>
    p match {
      case _: Prime => true
      case _ => false
    }
  } map (_.toPrime.asInstanceOf[Prime])

  def apply(x: BigInt): BigIntLike = MaybePrime(x)

  /**
   * Create an (optional) Mersenne Prime of form (2 to the power of the ith prime) - 1.
   *
   * @param i the index of the exponent of two.
   * @return an Option[Prime]
   */
  def createMersennePrime(i: Int): ScalaNumber = {
    val maybePrime = MaybePrime(mersenneNumber(i))
    val result = maybePrime.toPrime
    if (result.isInstanceOf[Prime]) println(s"Mersenne Prime number $i based on ${primes(i)}: $result")
    result
  }

  /**
   * Method to yield a Mersenne number: (2 to the power of the ith prime) - 1.
   *
   * @param i the index of the exponent of two.
   * @return a BigInt.
   */
  private def mersenneNumber(i: Int): BigInt = mersenneNumber(primes(i))

  /**
   * Method to yield a Mersenne number: (2 to the power of n) - 1.
   *
   * @param p the prime number to generate the Mersenne prime.
   *          NOTE that no explicit check is made to ensure that n is prime.
   * @return a BigInt.
   */
  private def mersenneNumber(p: Prime): BigInt = BigInt(2).pow(p.x.toInt) - 1
}

object Eratosthenes extends App {
  println(primes take 100 to List)
}