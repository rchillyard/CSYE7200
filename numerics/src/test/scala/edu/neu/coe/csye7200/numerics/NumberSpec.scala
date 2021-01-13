package edu.neu.coe.csye7200.numerics

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Failure, Left, Try}

class NumberSpec extends AnyFlatSpec with should.Matchers {

  private val numberOne = Number(1)
  private val bigOne = BigInt(1)
  private val ratOne = Rational.one
  private val doubleOne = 1.0

  behavior of "value"
  it should "yield Right(1)" in {
    val target = new Number(Right(1))
    target.value shouldBe Right(1)
  }
  it should "yield Left(Right(1))" in {
    val target = new Number(Left(Right(bigOne)))
    target.value shouldBe Left(Right(bigOne))
  }

  behavior of "toRational"
  it should "yield Some(1)" in {
    val target = new Number(Right(1))
    target.toRational shouldBe Some(ratOne)
  }

  behavior of "toString"
  it should "yield 1" in {
    val target = new Number(Right(1))
    target.toString shouldBe "1"
  }

  behavior of "toDouble"
  it should "yield 1" in {
    val target = new Number(Right(1))
    target.toDouble shouldBe Some(doubleOne)
  }

  behavior of "isValid"
  it should "yield true for 1" in {
    val target = new Number(Right(1))
    target.isValid shouldBe true
  }
  it should "yield false for None" in {
    val target = new Number(Left(Left(Left(None))))
    target.isValid shouldBe false
  }

  behavior of "parse"
  it should "work for 1" in {
    val xy: Try[Number] = Number.parse("1")
    xy.get shouldBe new Number(Right(1))
  }
  it should "work for 3/2" in {
    val xy: Try[Number] = Number.parse("3/2")
    xy.get shouldBe new Number(Left(Left(Right(Rational(3, 2)))))
  }
  it should "work for 3.1415927" in {
    val xy: Try[Number] = Number.parse("3.1415927")
    xy.get shouldBe new Number(Left(Left(Right(Rational(31415927, 10000000)))))
  }
  it should "work for BigInt" in {
    val xy: Try[Number] = Number.parse("2147483648")
    xy.get shouldBe new Number(Left(Right(BigInt(2147483648L))))
  }
  it should "work for Pi" in {
    val xy: Try[Number] = Number.parse("1" + Factor.sPi)
    xy.get shouldBe Number(1, Pi)
  }
  it should "work for pi" in {
    val xy: Try[Number] = Number.parse("1pi")
    xy.get shouldBe Number(1, Pi)
  }
  ignore should "work for e" in {
    val xy: Try[Number] = Number.parse("1" + Factor.sE)
    xy.get shouldBe Number(1, E)
  }
  it should "fail with x" in {
    Number.parse("1x") should matchPattern { case Failure(_) => }
  }

  behavior of "apply"
  it should "work for 1" in {
    val target = numberOne
    target.value shouldBe Right(1)
  }
  it should "work for BigInt(1)" in {
    val target = Number(bigOne)
    target.value shouldBe Left(Right(bigOne))
  }
  it should "work for Rational(1,2)" in {
    val target = Number(Rational(1, 2))
    target.value shouldBe Left(Left(Right(Rational(1, 2))))
  }
  it should "work for pi" in {
    val target = Number(Math.PI)
    target.value shouldBe Left(Left(Left(Some(Math.PI))))
  }
  it should "work for nothing" in {
    val target = Number()
    target.value shouldBe Left(Left(Left(None)))
  }
  it should "work for BigDecimal(3.1415927)" in {
    val target = Number(BigDecimal(3.1415927))
    target.value shouldBe Left(Left(Right(Rational(31415927, 10000000))))
  }

  behavior of "normalize"
  it should "work for 1" in {
    val target = numberOne
    target.specialize.value shouldBe Right(1)
  }
  it should "work for BigInt 1" in {
    val target = Number(bigOne)
    target.specialize.value shouldBe Right(1)
  }
  it should "work for BigInt(1+Int.MaxValue)" in {
    val bigInt = BigInt(1L + Int.MaxValue)
    val target = Number(bigInt)
    target.specialize.value shouldBe Left(Right(bigInt))
  }
  it should "work for Rational(1)" in {
    val target = Number(ratOne)
    target.specialize.value shouldBe Right(1)
  }
  it should "work for Rational.half" in {
    val target = Number(Rational.half)
    target.specialize.value shouldBe Left(Left(Right(Rational(1, 2))))
  }
  it should "work for 1.0" in {
    val target = Number(doubleOne)
    target.specialize.value shouldBe Right(1)
  }
  it should "work for pi" in {
    val target = Number(Math.PI)
    target.specialize.value shouldBe Left(Left(Left(Some(Math.PI))))
  }
  it should "work for nothing" in {
    val target = Number()
    target.specialize.value shouldBe Left(Left(Left(None)))
  }

  behavior of "scale"
  it should "work for Scalar, Scalar" in {
    val target = numberOne
    target.scale(Scalar) shouldBe numberOne
  }
  it should "work for Pi, Pi" in {
    val target = Number(1, Pi)
    target.scale(Pi) shouldBe target
  }
  it should "work for Scalar, Pi" in {
    val target = numberOne
    target.scale(Pi) shouldBe Number(1 / Math.PI, Pi)
  }
  it should "work for Pi, Scalar" in {
    val target = Number(1, Pi)
    println(target)
    target.scale(Scalar) shouldBe Number(Math.PI)
  }
  it should "work for Scalar, E" in {
    val target = numberOne
    target.scale(E) shouldBe Number(1 / Math.E, E)
  }
  it should "work for E, Scalar" in {
    val target = Number(1, E)
    target.scale(Scalar) shouldBe Number(Math.E)
  }
  it should "work for Pi, E" in {
    val target = Number(1, Pi)
    target.scale(E) shouldBe Number(Math.PI / Math.E, E)
  }
  it should "work for E, Pi" in {
    val target = Number(1, E)
    target.scale(Pi) shouldBe Number(Math.E / Math.PI, Pi)
  }

  behavior of "alignFactors"
  it should "work for Scalar, Scalar" in {
    val target = numberOne
    target.alignFactors(Number(2)) shouldBe(numberOne, Number(2))
  }
  it should "work for Scalar, Pi" in {
    val target = numberOne
    target.alignFactors(Number(2, Pi)) shouldBe(numberOne, Number(2 * Math.PI))
  }
  it should "work for Pi, Scalar" in {
    val target = Number(2, Pi)
    target.alignFactors(numberOne) shouldBe(Number(2 * Math.PI), numberOne)
  }
  it should "work for Pi, Pi" in {
    val target = Number(1, Pi)
    target.alignFactors(Number(2, Pi)) shouldBe(Number(1, Pi), Number(2, Pi))
  }

  behavior of "alignTypes"
  it should "work for Int,Int" in {
    val target = numberOne
    target.alignTypes(Number(2)) shouldBe(numberOne, Number(2))
  }
  it should "work for Int Int(Pi)" in {
    val target = numberOne
    target.alignTypes(Number(1, Pi)) shouldBe(numberOne, Number(1, Pi))
  }
  it should "work for Int,BigInt" in {
    val target = numberOne
    target.alignTypes(Number(BigInt(2))) shouldBe(Number(BigInt(2)), Number(bigOne))
  }
  it should "work for BigInt,Int" in {
    val target = Number(bigOne)
    target.alignTypes(Number(2)) shouldBe(Number(bigOne), Number(BigInt(2)))
  }
  it should "work for Int,Rational" in {
    val target = numberOne
    target.alignTypes(Number(Rational(2))) shouldBe(Number(Rational(2)), Number(Rational(1)))
  }
  it should "work for Rational,Int" in {
    val target = Number(Rational(1))
    target.alignTypes(Number(2)) shouldBe(Number(Rational(1)), Number(Rational(2)))
  }
  it should "work for Int,Double" in {
    val target = numberOne
    target.alignTypes(Number(2.0)) shouldBe(Number(2.0), Number(doubleOne))
  }
  it should "work for Double,Int" in {
    val target = Number(doubleOne)
    target.alignTypes(Number(2)) shouldBe(Number(doubleOne), Number(2.0))
  }
  it should "work for Int,None" in {
    val target = numberOne
    target.alignTypes(Number()) shouldBe(Number(), Number())
  }
  it should "work for None,Int" in {
    val target = Number()
    target.alignTypes(Number(2)) shouldBe(Number(), Number())
  }

  it should "work for BigInt,BigInt" in {
    val target = Number(bigOne)
    target.alignTypes(Number(BigInt(2))) shouldBe(Number(bigOne), Number(BigInt(2)))
  }
  it should "work for BigInt,Rational" in {
    val target = Number(bigOne)
    target.alignTypes(Number(Rational(2))) shouldBe(Number(Rational(2)), Number(Rational(1)))
  }
  it should "work for Rational,BigInt" in {
    val target = Number(Rational(1))
    target.alignTypes(Number(BigInt(2))) shouldBe(Number(Rational(1)), Number(Rational(2)))
  }
  it should "work for BigInt,Double" in {
    val target = Number(bigOne)
    target.alignTypes(Number(2.0)) shouldBe(Number(2.0), Number(doubleOne))
  }
  it should "work for Double,BigInt" in {
    val target = Number(doubleOne)
    target.alignTypes(Number(BigInt(2))) shouldBe(Number(doubleOne), Number(2.0))
  }
  it should "work for BigInt,None" in {
    val target = Number(bigOne)
    target.alignTypes(Number()) shouldBe(Number(), Number())
  }
  it should "work for None,BigInt" in {
    val target = Number()
    target.alignTypes(Number(BigInt(2))) shouldBe(Number(), Number())
  }

  it should "work for Rational,Rational" in {
    val target = Number(Rational(1))
    target.alignTypes(Number(Rational(2))) shouldBe(Number(Rational(1)), Number(Rational(2)))
  }
  it should "work for Rational,Double" in {
    val target = Number(Rational(1))
    target.alignTypes(Number(2.0)) shouldBe(Number(2.0), Number(doubleOne))
  }
  it should "work for Double,Rational" in {
    val target = Number(doubleOne)
    target.alignTypes(Number(Rational(2))) shouldBe(Number(doubleOne), Number(2.0))
  }
  it should "work for Rational,None" in {
    val target = Number(Rational(1))
    target.alignTypes(Number()) shouldBe(Number(), Number())
  }
  it should "work for None,Rational" in {
    val target = Number()
    target.alignTypes(Number(Rational(2))) shouldBe(Number(), Number())
  }

  it should "work for Double,Double" in {
    val target = Number(doubleOne)
    target.alignTypes(Number(2.0)) shouldBe(Number(doubleOne), Number(2.0))
  }
  it should "work for Double,None" in {
    val target = Number(doubleOne)
    target.alignTypes(Number()) shouldBe(Number(), Number())
  }
  it should "work for None,Double" in {
    val target = Number()
    target.alignTypes(Number(2.0)) shouldBe(Number(), Number())
  }

  it should "work for None,None" in {
    val target = Number()
    target.alignTypes(Number()) shouldBe(Number(), Number())
  }

  behavior of "plus"
  it should "add 1 and 2" in {
    val x = numberOne
    val y = Number(2)
    (x + y) shouldBe Number(3)
  }
  it should "add BigInt 1 and 2" in {
    val x = Number(bigOne)
    val y = Number(2)
    (x + y) shouldBe Number(3)
  }
  it should "add Rational 1 and 2" in {
    val x = Number(ratOne)
    val y = Number(2)
    (x + y) shouldBe Number(3)
  }
  it should "add Double 1 and 2" in {
    val x = Number(doubleOne)
    val y = Number(2)
    (x + y) shouldBe Number(3)
  }
  it should "add Double 1 and Pi" in {
    val x = Number(doubleOne)
    val y = Number(1, Pi)
    (x + y) shouldBe Number(Math.PI + 1)
  }
  it should "add Pi and 2Pi" in {
    val x = Number(1, Pi)
    val y = Number(2, Pi)
    (x + y) shouldBe Number(3, Pi)
  }

  behavior of "minus"
  it should "negate 1" in {
    val x = numberOne
    -x shouldBe Number(-1)
  }
  it should "negate BigInt 1" in {
    val x = Number(bigOne)
    -x shouldBe Number(BigInt(-1))
  }
  it should "negate Rational 1" in {
    val x = Number(ratOne)
    -x shouldBe Number(Rational(-1))
  }
  it should "negate Double 1" in {
    val x = Number(doubleOne)
    -x shouldBe Number(-doubleOne)
  }

  behavior of "subtract"
  it should "subtract 1 from 2" in {
    val x = Number(2)
    val y = numberOne
    (x - y) shouldBe numberOne
  }
  it should "subtract BigInt 1 from 2" in {
    val x = Number(BigInt(2))
    val y = numberOne
    (x - y) shouldBe numberOne
  }
  it should "subtract Rational 1 from 2" in {
    val x = Number(Rational(2))
    val y = numberOne
    (x - y) shouldBe numberOne
  }
  it should "subtract Double 1 from 2" in {
    val x = Number(2.0)
    val y = numberOne
    (x - y) shouldBe numberOne
  }

  behavior of "times"
  it should "multiply 1 and 2" in {
    val x = numberOne
    val y = Number(2)
    (x * y) shouldBe Number(2)
  }
  it should "multiply BigInt 1 and 2" in {
    val x = Number(bigOne)
    val y = Number(2)
    (x * y) shouldBe Number(2)
  }
  it should "multiply Rational 1 and 2" in {
    val x = Number(Rational(1))
    val y = Number(2)
    (x * y) shouldBe Number(2)
  }
  it should "multiply Double 1 and 2" in {
    val x = Number(doubleOne)
    val y = Number(2)
    (x * y) shouldBe Number(2)
  }
  it should "multiply 2 and Pi" in {
    val x = Number(2)
    val y = Number(1, Pi)
    (x * y) shouldBe Number(2, Pi)
  }
  it should "multiply Pi and 2" in {
    val x = Number(1, Pi)
    val y = Number(2)
    (x * y) shouldBe Number(2, Pi)
  }

  behavior of "invert"
  it should "invert 1" in {
    val x = numberOne
    x.invert shouldBe numberOne
  }
  it should "invert BigInt 1" in {
    val x = Number(bigOne)
    x.invert shouldBe numberOne
  }
  it should "invert 2" in {
    val x = Number(2)
    x.invert shouldBe Number(Rational.half)
  }
  it should "invert BigInt 2" in {
    val x = Number(BigInt(2))
    x.invert shouldBe Number(Rational.half)
  }
  it should "invert Rational 2" in {
    val x = Number(Rational.two)
    x.invert shouldBe Number(Rational.half)
  }
  it should "invert Double 2" in {
    val x = Number(2.0)
    x.invert shouldBe Number(0.5)
  }

  behavior of "division"
  it should "divide 1 by 2" in {
    val x = numberOne
    val y = Number(2)
    (x / y) shouldBe Number(Rational.half)
  }
  it should "divide BigInt 1 by 2" in {
    val x = Number(bigOne)
    val y = Number(2)
    (x / y) shouldBe Number(Rational.half)
  }
  it should "divide Rational 1 by 2" in {
    val x = Number(Rational(1))
    val y = Number(2)
    (x / y) shouldBe Number(Rational.half)
  }
  it should "divide Double 1 by 2" in {
    val x = Number(doubleOne)
    val y = Number(2)
    (x / y) shouldBe Number(0.5)
  }

  // Following are the tests of Ordering[Number]

  behavior of "compare"
  it should "work for 1, 1" in {
    val x = numberOne
    val y = numberOne
    implicitly[Numeric[Number]].compare(x, y) shouldBe 0
  }
  it should "work for 1, 2" in {
    val x = numberOne
    val y = Number(2)
    implicitly[Numeric[Number]].compare(x, y) shouldBe -1
  }
  it should "work for 2, 1" in {
    val x = Number(2)
    val y = numberOne
    implicitly[Numeric[Number]].compare(x, y) shouldBe 1
  }
  it should "work for BigInt 1, 1" in {
    val x = Number(bigOne)
    val y = Number(bigOne)
    implicitly[Numeric[Number]].compare(x, y) shouldBe 0
  }
  it should "work for BigInt 1, 2" in {
    val x = Number(bigOne)
    val y = Number(BigInt(2))
    implicitly[Numeric[Number]].compare(x, y) shouldBe -1
  }
  it should "work for BigInt 2, 1" in {
    val x = Number(BigInt(2))
    val y = Number(bigOne)
    implicitly[Numeric[Number]].compare(x, y) shouldBe 1
  }
  it should "work for Rational 1, 1" in {
    val x = Number(ratOne)
    val y = Number(ratOne)
    implicitly[Numeric[Number]].compare(x, y) shouldBe 0
  }
  it should "work for Rational 1, 2" in {
    val x = Number(ratOne)
    val y = Number(Rational.two)
    implicitly[Numeric[Number]].compare(x, y) shouldBe -1
  }
  it should "work for Rational 2, 1" in {
    val x = Number(Rational.two)
    val y = Number(ratOne)
    implicitly[Numeric[Number]].compare(x, y) shouldBe 1
  }
  it should "work for Double 1, 1" in {
    val x = Number(doubleOne)
    val y = Number(doubleOne)
    implicitly[Numeric[Number]].compare(x, y) shouldBe 0
  }
  it should "work for Double 1, 2" in {
    val x = Number(doubleOne)
    val y = Number(2.0)
    implicitly[Numeric[Number]].compare(x, y) shouldBe -1
  }
  it should "work for Double 2, 1" in {
    val x = Number(2.0)
    val y = Number(doubleOne)
    implicitly[Numeric[Number]].compare(x, y) shouldBe 1
  }

  behavior of "toInt"
  it should "work for 1" in {
    val target = numberOne
    target.toInt shouldBe Some(1)
  }
  it should "work for BigInt 1" in {
    val target = Number(bigOne)
    target.toInt shouldBe Some(1)
  }
  it should "work for Rational 1" in {
    val target = Number(ratOne)
    target.toInt shouldBe Some(1)
  }
  ignore should "work for too big" in {
    val target = Number(BigInt(2147483648L))
    target.toInt shouldBe None
  }
  it should "work for 3.14..." in {
    val target = Number(3.1415927)
    target.toInt shouldBe None
  }
  it should "work for pi" in {
    val target = Number(1, Pi)
    target.toInt shouldBe None
  }

  behavior of "toBigInt"
  it should "work for 1" in {
    val target = numberOne
    target.toBigInt shouldBe Some(BigInt(1L))
  }
  it should "work for BigInt 1" in {
    val target = Number(bigOne)
    target.toBigInt shouldBe Some(BigInt(1L))
  }
  it should "work for Rational 1" in {
    val target = Number(ratOne)
    target.toBigInt shouldBe Some(BigInt(1L))
  }
  it should "work for pi" in {
    val target = Number(Math.PI)
    target.toBigInt shouldBe None
  }

  // Following are the tests of Numeric[Number]

  behavior of "Numeric toInt"
  it should "work for 1" in {
    val target = numberOne
    implicitly[Numeric[Number]].toInt(target) shouldBe 1
  }
  it should "work for BigInt 1" in {
    val target = Number(bigOne)
    implicitly[Numeric[Number]].toInt(target) shouldBe 1
  }
  it should "work for Rational 1" in {
    val target = Number(ratOne)
    implicitly[Numeric[Number]].toInt(target) shouldBe 1
  }
  it should "work for 1.0" in {
    val target = Number(doubleOne)
    implicitly[Numeric[Number]].toInt(target) shouldBe 1
  }

  behavior of "Numeric toLong"
  it should "work for 1" in {
    val target = numberOne
    implicitly[Numeric[Number]].toLong(target) shouldBe 1L
  }
  it should "work for BigInt 1" in {
    val target = Number(bigOne)
    implicitly[Numeric[Number]].toLong(target) shouldBe 1L
  }
  it should "work for Rational 1" in {
    val target = Number(ratOne)
    implicitly[Numeric[Number]].toLong(target) shouldBe 1L
  }
  it should "work for 1.0" in {
    val target = Number(doubleOne)
    implicitly[Numeric[Number]].toLong(target) shouldBe 1L
  }

  behavior of "Numeric toDouble"
  it should "work for 1" in {
    val target = numberOne
    implicitly[Numeric[Number]].toDouble(target) shouldBe 1.0
  }
  it should "work for BigInt 1" in {
    val target = Number(bigOne)
    implicitly[Numeric[Number]].toDouble(target) shouldBe 1.0
  }
  it should "work for Rational 1" in {
    val target = Number(ratOne)
    implicitly[Numeric[Number]].toDouble(target) shouldBe 1.0
  }
  it should "work for 1.0" in {
    val target = Number(doubleOne)
    implicitly[Numeric[Number]].toDouble(target) shouldBe 1.0
  }
}
