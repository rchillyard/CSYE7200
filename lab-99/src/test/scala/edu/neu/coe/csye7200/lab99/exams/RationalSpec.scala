package edu.neu.coe.csye7200.lab99.exams

import edu.neu.coe.csye7200.lab99.exams.TryOps.{toTry, toTryWithRationalException, toTryWithThrowable}
import edu.neu.coe.csye7200.lab99.exams.Rational.{RationalHelper, negZero}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Failure, Success}

class RationalSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Rational"

  it should "apply" in {
    val half = Rational(2,4)
    half.n shouldBe 1
    half.d shouldBe 2
  }

  it should "use the interpolator" in {
    val half = r"1/2"
    half.n shouldBe 1
    half.d shouldBe 2
  }

  it should "use the interpolator (2)" in {
    val one = 1
    val two = 2
    val half = r"$one/$two"
    half.n shouldBe 1
    half.d shouldBe 2
  }

  behavior of "plus"

  it should "work" in {
    val r = Rational(2, 10) + Rational(1, 10)
    r shouldBe Rational(3,10)
    negZero + r shouldBe r
    r + negZero shouldBe r
  }

  behavior of "power"

  it should "power 1" in {
    Rational.one power 0 shouldBe Rational.one
    Rational.two power 0 shouldBe Rational.one
    val r: Rational = 1L
    r power 0 shouldBe Rational.one
  }

  it should "power 2" in {
    Rational.one power 1 shouldBe Rational.one
    Rational.two power 1 shouldBe Rational.two
    val r: Rational = BigInt(1)
    r power 1 shouldBe Rational.one
  }

  it should "power 3" in {
    val r = Rational(4)
    val p = r"3/2"
    val xy = r power p
    xy.isSuccess shouldBe true
    val x = xy.get
    x shouldBe Rational(8,1)
    x power p.invert shouldBe Success(r)
  }

  it should "power 4" in {
    val r = Rational.one
    val p = r"3/2"
    val xo = r power p
    xo.isSuccess shouldBe true
    val x = xo.get
    x shouldBe Rational.one
  }

  it should "fail with bad power" in {
    val r = Rational(3)
    val p = r"3/2"
    val x = r power p
    x.isSuccess shouldBe false
  }

  behavior of "root"

  it should "root 0" in {
    Rational.one root 0 shouldBe Some(Rational.one)
    Rational.two root 0 shouldBe None
  }

  it should "root 1" in {
    Rational.one root 1 shouldBe Some(Rational.one)
    Rational.two root 1 shouldBe Some(Rational.two)
  }

  it should "root 2" in {
    Rational.one root 2 shouldBe Some(Rational.one)
    Rational(4) root 2 shouldBe Some(Rational.two)
    Rational.two root 2 shouldBe None
  }

  it should "root -2" in {
    Rational.one root -2 shouldBe Some(Rational.one)
    Rational(4) root -2 shouldBe None
    Rational.two root -2 shouldBe None
  }

  behavior of "TryOps"

  private val hello = "hello"

  it should "toTry" in {
    toTry(Some(1), Failure(new NoSuchElementException)) should matchPattern { case Success(1) => }
    toTry(None, Failure(new NoSuchElementException)) should matchPattern { case Failure(_: NoSuchElementException) => }
  }

  it should "toTryWithThrowable" in {
    toTryWithThrowable(Some(1), RationalException(hello)) should matchPattern { case Success(1) => }
    toTryWithThrowable(None, RationalException(hello)) should matchPattern { case Failure(RationalException(`hello`)) => }
  }

  it should "toTryWithRationalException" in {
    toTryWithRationalException(Some(1), hello) should matchPattern { case Success(1) => }
    toTryWithRationalException(None, hello) should matchPattern { case Failure(RationalException(`hello`)) => }
  }
}