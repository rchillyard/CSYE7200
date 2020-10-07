package edu.neu.coe.csye7200.numerics

import edu.neu.coe.csye7200.numerics.Rational.RationalHelper
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class InfiniteSumSpec extends AnyFlatSpec with should.Matchers {

  behavior of "harmonic series"

  val f1: (Long, Rational) => Rational = (i, _) => Rational(i + 1).invert
  val EulerMascheroni = 0.5772156649015328606

  it should "asLazyList" in {
    InfiniteSum[Rational](Rational.zero, f1).asLazyList.take(3).toList shouldBe List(Rational.zero, Rational.one, Rational.half)
  }

  it should "apply" in {
    val target = InfiniteSum[Rational](Rational.zero, f1)
    val expected = List[Rational](r"3/2", r"11/6", r"25/12", r"137/60")
    for (i <- 0 to 2) target(i + 3) shouldBe expected(i)
  }


  it should "toDouble" in {
    val target = InfiniteSum[Rational](Rational.zero, f1)
    check(target, math.log(_) + EulerMascheroni, 10, 6E-2)
    check(target, math.log(_) + EulerMascheroni, 20, 6E-2)
    check(target, math.log(_) + EulerMascheroni, 30, 6E-2)
    check(target, math.log(_) + EulerMascheroni, 50, 6E-2)
    check(target, math.log(_) + EulerMascheroni, 100, 6E-2)
  }

  behavior of "Leibniz"

  val f2: (Long, Rational) => Rational = (i, _) => Rational.one.negate.power((i % 2).toInt) * Rational(2 * i + 1).invert
  val PiBy4: Double = math.Pi / 4

  it should "asLazyList" in {
    InfiniteSum[Rational](Rational.zero, f2).asLazyList.take(3).toList shouldBe List(Rational.zero, Rational.one, r"-1/3")
  }

  it should "apply" in {
    val target = InfiniteSum[Rational](Rational.zero, f2)
    val expected = List[Rational](r"2/3", r"13/15", r"76/105", r"263/315")
    for (i <- 0 to 2) target(i + 3) shouldBe expected(i)
  }


  it should "toDouble" in {
    val target = InfiniteSum[Rational](Rational.zero, f2)
    check(target, _ => PiBy4, 10, 6E-2)
    check(target, _ => PiBy4, 20, 2E-2)
    check(target, _ => PiBy4, 30, 1E-2)
    check(target, _ => PiBy4, 50, 6E-3)
    check(target, _ => PiBy4, 100, 3E-3)
  }

  behavior of "Euler"

  /**
    * The function returned is a particular case of the Riemann zeta function where z is a real positive integer and a multiple of two.
    */
  def riemannPosEvenInt(n: Int): (Long, Rational) => Rational = (i, _) => Rational(i + 1).invert.power(2 * n)

  val f3: (Long, Rational) => Rational = riemannPosEvenInt(1)
  val Basel: Double = math.Pi * math.Pi / 6

  it should "asLazyList" in {
    InfiniteSum[Rational](Rational.zero, f3).asLazyList.take(4).toList shouldBe List(Rational.zero, Rational.one, r"1/4", r"1/9")
  }

  it should "apply" in {
    val target = InfiniteSum[Rational](Rational.zero, f3)
    val expected = List[Rational](Rational.zero, Rational.one, r"5/4", r"49/36", r"205/144")
    for (i <- 0 to 4) target(i + 1) shouldBe expected(i)
  }


  it should "toDouble" in {
    val target = InfiniteSum[Rational](Rational.zero, f3)
    check(target, _ => Basel, 10, 6E-1)
    check(target, _ => Basel, 20, 2E-1)
    check(target, _ => Basel, 30, 1E-1)
    check(target, _ => Basel, 50, 6E-2)
    check(target, _ => Basel, 100, 3E-2)
  }

  behavior of "Stefan-Boltzmann"

  /**
    * It's amazing how all these things are connected.
    * See https://en.wikipedia.org/wiki/Stefan–Boltzmann_law.
    * The famous fourth-power law of Stefan-Boltzmann actually includes the value of this infinite series in its constant of proportionality.
    * In particular, the constant is zeta4 * 8 pi * k^4^ / c^2^ / h^3^.
    */
  val f4: (Long, Rational) => Rational = riemannPosEvenInt(2)
  val zeta4: Double = math.Pi * math.Pi * math.Pi * math.Pi / 90

  it should "asLazyList" in {
    InfiniteSum[Rational](Rational.zero, f4).asLazyList.take(4).toList shouldBe List(Rational.zero, Rational.one, r"1/16", r"1/81")
  }

  it should "apply" in {
    val target = InfiniteSum[Rational](Rational.zero, f4)
    val expected = List[Rational](Rational.zero, Rational.one, r"17/16")
    for (i <- 0 to 2) target(i + 1) shouldBe expected(i)
  }

  it should "toDouble" in {
    val target = InfiniteSum[Rational](Rational.zero, f4)
    check(target, _ => zeta4, 100, 6E-7)
  }

  private def check(infiniteSum: InfiniteSum[Rational], expected: Int => Double, m: Int, epsilon: Double) = infiniteSum.toDouble(m) shouldBe expected(m) +- epsilon

}
