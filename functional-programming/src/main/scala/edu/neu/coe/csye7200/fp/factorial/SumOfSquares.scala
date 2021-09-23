package edu.neu.coe.csye7200.fp.factorial


import edu.neu.coe.csye7200.fp.Args
import scala.util.{Failure, Success, Try}

trait Ring[X] {
  def plus(x1: X, x2: X): X

  def times(x1: X, x2: X): X
}

object DoubleRing {

  trait DoubleRing extends Ring[Double] {
    def plus(x1: Double, x2: Double): Double = x1 + x2

    def times(x1: Double, x2: Double): Double = x1 * x2
  }

  implicit object DoubleRing extends DoubleRing

}

/**
  * NOTE: this only works for integers up to and including 1000000
  *
  * Created by scalaprof on 10/28/16.
  */
object SumOfSquares extends App {
  def sumOfSquares(xs: Seq[Long]) = xs.map(x => x * x).foldLeft[BigInt](BigInt(0))(_ + _)
//  def sumOfSquares(n: Int): BigInt = sumOfSquares(Stream.from(1).map(_.toLong).take(n))


  def sumOfSquares(n: Long): BigInt = {
    @scala.annotation.tailrec
    def inner(r: BigInt, x: Long): BigInt = if (x <= 0) r else inner(r + x * x, x - 1)

    inner(BigInt(0), n)
  }

  // The following version is limited to 1 million
  //  def sumOfSquares(xs: Seq[Long]) = xs map(x => x*x) sum

  // Syntax: sumOfSquares N | x1, x2, x3 ... xN
  // If N is provided then the sequence to be processed is the numbers 1 through N
  // Else if x1, x2... are provided, then these will be the sequence
  // Unless said sequence is empty, in which case we use 1, 2, ... 5 as a default
  private val lsy: Try[Seq[Long]] = Args.parse(args).map { x: String => x.toLong }.process(Map[String, Option[Long] => Unit]())
  private val (no, xs) = createXtuple[Long](lsy, (Some(5), Seq(1, 2, 3, 4, 5)))
  private val result: BigInt = sumOfSquares(xs)
  println(s"sum of $xs is $result")
  no match {
    case Some(n: Long) =>
      val expected = BigInt(n) * (n + 1) * (2 * n + 1) / 6
      if (result == expected) println("... which is correct")
      else println(s"but it should be $expected")
    case None =>
  }

  private def createXtuple[X: Numeric](xsy: Try[Seq[X]], defaultValue: (Some[X], Seq[X])): (Option[X], Seq[X]) = xsy match {
    case Success(xs) =>
      val xn = implicitly[Numeric[X]]
      if (xs.size == 1) (Some(xs.head), (1 to xn.toInt(xs.head)).map(xn.fromInt))
      else if (xs.nonEmpty) (None, xs)
      else defaultValue
    case Failure(x) => throw x
  }

}
