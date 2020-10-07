package edu.neu.coe.csye7200.numerics

import edu.neu.coe.csye7200.numerics.Rational.RationalHelper
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class InfiniteProductSpec extends AnyFlatSpec with should.Matchers {

  private def nextMultipleOfFour(i: Int) = (i + 1) / 4 * 4

  behavior of "Leibniz"

  val f2: Int => Rational = i => {
    val m = nextMultipleOfFour(i)
    Rational(i, if (m > 0) m else i)
  }
  val PiBy4: Double = math.Pi / 4

  it should "asLazyList" in {
    InfiniteProduct[Rational](f2).asLazyList.take(5).toList shouldBe List(Rational.one, r"3/4", r"5/4", r"7/8", r"11/12")
  }

  it should "apply" in {
    val target = InfiniteProduct[Rational](f2)
    val expected = List[Rational](1, r"3/4", r"15/16", r"15/16" * r"7/8", r"15/16" * r"7/8" * r"11/12", r"15/16" * r"7/8" * r"11/12" * r"13/12")
    for (i <- 0 to 5) target(i + 1) shouldBe expected(i)
  }


  it should "toDouble" in {
    val target = InfiniteProduct[Rational](f2)
    check(target, _ => PiBy4, 10, 6E-2)
    check(target, _ => PiBy4, 20, 2E-2)
    check(target, _ => PiBy4, 30, 1E-1)
    check(target, _ => PiBy4, 50, 6E-2)
    check(target, _ => PiBy4, 100, 3E-2)
  }

  behavior of "Euler"

  /**
    * This function gives us the series for a particular case of the Riemann zeta function where z is two.
    */
  val f3: Int => Rational = i => {
    val m = i * i
    Rational(m, m - 1)
  }
  val Basel: Double = math.Pi * math.Pi / 6

  it should "asLazyList" in {
    InfiniteProduct[Rational](f3).asLazyList.take(4).toList shouldBe List(r"4/3", r"9/8", r"25/24", r"49/48")
  }

  it should "apply" in {
    val target = InfiniteProduct[Rational](f3)
    val expected = List[Rational](r"4/3", r"3/2", r"25/16", r"25/16" * r"49/48", r"25/16" * r"49/48" * r"121/120")
    for (i <- 0 to 4) target(i + 1) shouldBe expected(i)
  }


  it should "toDouble" in {
    val target = InfiniteProduct[Rational](f3)
    check(target, _ => Basel, 10, 6E-1)
    check(target, _ => Basel, 20, 2E-1)
    check(target, _ => Basel, 30, 1E-1)
    check(target, _ => Basel, 50, 6E-2)
    check(target, _ => Basel, 100, 3E-2)
  }

  private def check(InfiniteProduct: InfiniteProduct[Rational], expected: Int => Double, m: Int, epsilon: Double) = InfiniteProduct.toDouble(m) shouldBe expected(m) +- epsilon

}
