package edu.neu.coe.csye7200.numerics

/**
  * This case class represents an infinite sum of terms which can be derived by a function (f) which
  * takes as its parameters the index of the term and the previous value of the term.
  *
  * @param x0 the initial value of type X which is used for the first term.
  * @param f  the function (see description above).
  * @tparam X a type for which there is evidence of Numeric[X].
  */
case class InfiniteSum[X: Numeric](x0: X, f: (Long, X) => X) extends (Int => X) {

  private val xn = implicitly[Numeric[X]]

  private def getNext(i: Long, x: X): (Long, X) = (i + 1, f(i, x))

  private val g = (getNext _).tupled

  /**
    * A lazy val representing the LazyList[X] which is all of the terms of the infinite sum.
    */
  lazy val asLazyList: LazyList[X] = LazyList.iterate[(Long, X)]((0L, x0))(g).map(_._2)

  /**
    * The apply function which takes a number n and returns a value of X representing the (finite) sum of the
    * first n terms of the infinite series.
    *
    * @param n the number which specifies how many terms we should use to evaluate this infinite sum.
    * @return an X which represents an approximation to the infinite sum.
    */
  override def apply(n: Int): X = asLazyList.take(n).foldLeft(xn.zero)(xn.plus)

  /**
    * A function which takes a number n and returns a Double representing the (finite) sum of the
    * first n terms of the infinite series.
    *
    * @param n the number which specifies how many terms we should use to evaluate this infinite sum.
    * @return a Double which represents an approximation to the infinite sum. This values depends on #apply(n).
    */
  def toDouble(n: Int): Double = xn.toDouble(apply(n))
}
