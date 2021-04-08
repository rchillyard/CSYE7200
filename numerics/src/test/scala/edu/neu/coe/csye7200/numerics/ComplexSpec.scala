package edu.neu.coe.csye7200.numerics

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ComplexSpec extends AnyFlatSpec with should.Matchers {

  implicit val ok: Boolean = true

  behavior of "ComplexSpec"

  it should "apply(2)" in {
    val two = Complex(2)
    two.real shouldBe 2.0
    two.imag shouldBe 0.0
  }

  it should "parseString" in {
    implicitly[Numeric[Complex]].parseString("1i1") shouldBe Some(Complex(1,1))
    implicitly[Numeric[Complex]].parseString("1") shouldBe Some(Complex(1,0))
  }

  it should "apply (polar)" in {
    val target = Complex.create((1, math.Pi / 2))
    target.real shouldBe 0.0 +- 1E-15
    target.imag shouldBe 1.0 +- 1E-15
    (target - Complex.i).modulus shouldBe 0.0 +- 1E-14
  }

  it should "plus" in {
    Complex.one + Complex(2) should matchPattern { case Complex(3, 0) => }
    Complex(1, 1) + Complex(2) should matchPattern { case Complex(3, 1) => }
    Complex(1, 1) + Complex(2, 1) should matchPattern { case Complex(3, 2) => }
    Complex.one + Complex.i should matchPattern { case Complex(1, 1) => }
    Complex.one + Complex.i + Complex.i should matchPattern { case Complex(1, 2) => }
  }

  it should "times" in {
    Complex.one * Complex(2) should matchPattern { case Complex(2, 0) => }
    Complex(1, 1) * Complex(2) should matchPattern { case Complex(2, 2) => }
    Complex(1, 1) * Complex(2, 1) should matchPattern { case Complex(1, 3) => }
    Complex.one * Complex.i should matchPattern { case Complex(0, 1) => }
    Complex.one * Complex.i * Complex.i should matchPattern { case Complex(-1, 0) => }
  }

  it should "sum" in {
    val xs = List(Complex.one, Complex.i, Complex.minusOne)
    xs.sum shouldBe Complex.i
  }

  it should "moveHorizontal" in {
    val x = Complex.one
    x.moveHorizontal(1) shouldBe Complex(2,0)
  }

  it should "moveVertical" in {
    val x = Complex.one
    x.moveVertical(1) shouldBe Complex(1,1)
  }

  behavior of "HasImaginary"
  it should "work" in {
    def complexConjugate[T: HasImaginary](t: T): T = implicitly[HasImaginary[T]].conjugate(t)

    complexConjugate(Complex.i) * Complex.i shouldBe Complex.one
  }
}
