package edu.neu.coe.csye7200

/**
  * @author scalaprof
  */
object Prime extends App {
  // TESTME
  val start = System.currentTimeMillis()

  // TESTME
  //  def nthPrime(n: Int) = Stream.from(2).filter(isPrime(_))(n) // 46 mSec
  def nthPrime(n: Int) = (2 to 200000).filter(isPrime)(n)

  // TESTME
  // 104 mSec
  def isPrime(x: Int): Boolean = (2 to math.floor(math.sqrt(x)).toInt) find {
    x % _ == 0
  } match {
    case Some(_) => false;
    case None => true
  }

  lazy val allPrimes: LazyList[BigInt] = LazyList.from(2).map(BigInt(_)).filter(_.isProbablePrime(30))

  /**
   * Method to yield a Map of prime factors.
   *
   * XXX Adapted from Scala 99: http://aperiodic.net/phil/scala/s-99/
   *
   * @return a Map of Prime -=> Int where the Int represents the number of times the factor is multiplied.
   */
  def primeFactorMultiplicity(x: BigInt): Map[BigInt, Int] = {
    def factorCount(n: BigInt, p: BigInt): (Int, BigInt) =
      if (n % p != 0) (0, n)
      else factorCount(n / p, p) match {
        case (c, d) => (c + 1, d)
      }

    def factorsR(n: BigInt, ps: LazyList[BigInt]): Map[BigInt, Int] =
      if (n == 1) Map()
      else if (n.isProbablePrime(20)) Map(n -> 1)
      else {
        val nps = ps.dropWhile(n % _ != 0)
        val (count, dividend) = factorCount(n, nps.head)
        Map(nps.head -> count) ++ factorsR(dividend, nps.tail)
      }

    factorsR(x, allPrimes)
  }


  val n = 9591
  val m = 100
  lazy val pN: Unit = for (_ <- 1 to m) nthPrime(n)
  val end = System.currentTimeMillis()
  println(s"${n}th prime: " + pN)
  println(s"time taken: ${(end - start) / m} mSec")
}