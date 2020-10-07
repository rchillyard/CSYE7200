package edu.neu.coe.csye7200.numerics

/**
  * This case class represents an infinite product of terms which can be derived by a function (f) which
  * takes as its parameter a prime number.
  *
  * Each successive term in the product is based on the list of primes starting with 2.
  *
  * @param f the function (see description above).
  * @tparam X a type for which there is evidence of Numeric[X].
  */
case class InfiniteProduct[X: Numeric](f: Int => X) extends (Int => X) {

  // NOTE: this is currently finite.
  private def primes: LazyList[Int] = LazyList(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199)

  private val xn = implicitly[Numeric[X]]

  /**
    * A lazy val representing the LazyList[X] which is all of the terms of the infinite product.
    */
  lazy val asLazyList: LazyList[X] = primes.map(f)

  /**
    * The apply function which takes a number n and returns a value of X representing the (finite) product of the
    * first n terms of the infinite series.
    *
    * @param n the number which specifies how many terms we should use to evaluate this infinite product.
    * @return an X which represents an approximation to the infinite product.
    */
  override def apply(n: Int): X = asLazyList.take(n).foldLeft(xn.one)(xn.times)

  /**
    * A function which takes a number n and returns a Double representing the (finite) product of the
    * first n terms of the infinite series.
    *
    * @param n the number which specifies how many terms we should use to evaluate this infinite product.
    * @return a Double which represents an approximation to the infinite product. This values depends on #apply(n).
    */
  def toDouble(n: Int): Double = xn.toDouble(apply(n))
}
