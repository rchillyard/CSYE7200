package edu.neu.coe.csye7200.prime

import com.typesafe.scalalogging.Logger
import edu.neu.coe.csye7200.prime.Prime.primes
import edu.neu.coe.csye7200.primeAlt.Prime.logger

case class Prime(x: BigInt, var valid: Option[Boolean]) {

  /**
   * Potentially mutating method to determine if this Prime is truly prime.
   * If necessary, this method invokes validate.
   *
   * @return a Boolean.
   */
  def isPrime: Boolean = valid getOrElse validate

  override def equals(obj: Any): Boolean = obj.getClass == classOf[Prime] && x == obj.asInstanceOf[Prime].x

  override def hashCode(): Int = x.hashCode()

  override def toString: String = s"""$x ${
    valid match {
      case Some(true) => "";
      case Some(false) => "composite";
      case _ => "?"
    }
  }"""

  /**
   * Mutating method to determine if this Prime is truly prime.
   * As a side-effect, <code>valid</code> will be set to Some(true) if truly prime, otherwise, Some(false)
   *
   * @return a Boolean.
   */
  private def validate: Boolean = {

    valid match {
      case None =>
        def composite = primes.takeWhile(p => p.x * p.x <= x).exists(p => x % p.x == 0)
        val isPrime = x == BigInt(2) || x == BigInt(3) || !composite
        logger.whenDebugEnabled(if (isPrime) logger.debug(s"$x is prime"))
        valid = Some(isPrime)
        isPrime
      case _ =>
        isPrime
    }
  }
}

object Prime {

  val logger: Logger = com.typesafe.scalalogging.Logger(classOf[Prime])

  def apply(x: BigInt): Prime = new Prime(x, None)

  val probablePrimes: LazyList[Prime] = (BigInt(2L) #:: LazyList.iterate(BigInt(3))(b => b + 2)) filter (_.isProbablePrime(20)) map (Prime(_))

  val primes: LazyList[Prime] = probablePrimes filter (_.isPrime)


  /**
   * Create an (optional) Mersenne Prime of form (2 to the power of the ith prime) - 1.
   *
   * @param i the index of the exponent of two.
   * @return an Option[Prime]
   */
  def createMersennePrime(i: Int): Prime = {
    val prime = Prime(mersenneNumber(i))
    if (prime.isPrime)
      println(s"Mersenne Prime number $i based on ${primes(i)}: $prime")
    prime
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
  println(primes take 10 to List)
}