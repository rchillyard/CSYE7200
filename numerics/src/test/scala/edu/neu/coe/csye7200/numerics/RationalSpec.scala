package edu.neu.coe.csye7200.numerics

import edu.neu.coe.csye7200.numerics.Rational.RationalHelper
import org.scalatest.matchers.should
import org.scalatest.{PrivateMethodTester, flatspec}

import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

/**
  * @author scalaprof
  */
class RationalSpec extends flatspec.AnyFlatSpec with should.Matchers with PrivateMethodTester {

  behavior of "new Rational(BigInt,Long)"

  it should "work for 0, 1" in {
    val r = new Rational(BigInt(0), 1L)
    r.isZero shouldBe true
    r shouldBe Rational(Rational.bigZero, 1)
  }
  it should "yield infinity for 1, 0" in {
    val r = new Rational(BigInt(1), 0L)
    r.isInfinity shouldBe true
  }
  it should "yield NaN for 0, 0" in {
    new Rational(BigInt(0), 0L).isNaN shouldBe true
  }
  it should "fail for 2, 2" in {
    a[IllegalArgumentException] should be thrownBy new Rational(BigInt(2), 2L)
  }
  it should "fail for 0, -1" in {
    a[IllegalArgumentException] should be thrownBy new Rational(BigInt(0), -1L)
  }

  behavior of "apply(BigInt,Long)"
  it should "work for 0, 1" in {
    Rational(BigInt(0), 1L) shouldBe Rational(Rational.bigZero, 1)
  }
  it should "succeed for 2, 2" in {
    Rational(BigInt(2), 2L) shouldBe Rational.one
  }
  it should "succeed for -1, -1" in {
    Rational(BigInt(-1), -1L) shouldBe Rational.one
  }
  it should "work for 355, 113" in {
    val a: BigInt = 355
    val b: Int = 113
    val r = Rational(a, b)
    Rational.hasCorrectRatio(r, a, b) shouldBe true
  }
  it should "work for 355*2, 113*2" in {
    val a: BigInt = 355 * 2
    val b: Int = 113 * 2
    val r = Rational(a, b)
    Rational.hasCorrectRatio(r, a, b) shouldBe true
  }
  it should "work for -355, -113" in {
    val a: BigInt = -355
    val b: Int = -113
    val r = Rational(a, b)
    Rational.hasCorrectRatio(r, a, b) shouldBe true
  }

  behavior of "apply(Long,Long)"
  it should "work for 0, 1" in {
    Rational(0L, 1L) shouldBe Rational(Rational.bigZero, 1)
  }
  behavior of "apply(Long,Int)"
  it should "work for 0, 1" in {
    Rational(0L, 1) shouldBe Rational(Rational.bigZero, 1)
  }
  behavior of "apply(Int,Int)"
  it should "work for 0, 1" in {
    Rational(0, 1) shouldBe Rational(Rational.bigZero, 1)
  }
  it should "work for 0, 0" in {
    Rational(0, 0) shouldBe Rational.NaN
  }
  it should "work for 1, 0" in {
    val r = Rational(1, 0)
    r.isInfinity shouldBe true
    r shouldBe Rational.infinity
    r.toString shouldBe "+ve infinity"
  }
  it should "work for -1, 0" in {
    val r = Rational(-1, 0)
    r.isInfinity shouldBe true
    r.toString shouldBe "-ve infinity"
  }
  it should "work for -1, -2" in {
    val r = Rational(-1, -2)
    r.isInfinity shouldBe false
    r shouldBe Rational.half
  }
  it should "work for -2624712818L, -1" in {
    val r = Rational(BigInt(-2624712818L), -1)
    r.signum shouldBe 1
  }

  it should "fail to convert to BigInt" in {
    val r = Rational(-1, 0)
    val decorateToBigInt = PrivateMethod[Try[BigInt]](Symbol("toBigInt"))
    val z: Try[BigInt] = Rational invokePrivate decorateToBigInt(r)
    a[RationalException] should be thrownBy z.get
  }

  behavior of "apply(Long)"
  it should "work for Rational.bigZero" in {
    Rational(0L) shouldBe new Rational(Rational.bigZero, 1)
  }

  behavior of "apply(Int)"
  it should "work for 0" in {
    Rational(0)
  }

  behavior of "apply(Double)"
  it should "convert Avagadro's number" in {
    val avagadro = Rational("6.02214076E23")
    avagadro shouldBe Rational(6.02214076E23) +- 1E9
  }
  it should "convert a very small number" in {
    val verySmall = Rational(-4.076956044934884E-134)
    Rational(-4.076956044934884E-134) shouldBe verySmall
  }
  it should "be zero for very very small number" in {
    val x = 1.111314801067662E-299
    val epsilon: Double = 1E-305
    val r = Rational(x)
    r.toDouble shouldBe x +- epsilon
  }

  behavior of "apply(BigDecimal)"
  it should "convert 0.5 to Rational" in {
    Rational(BigDecimal.valueOf(0.5)) shouldBe Rational.half
  }
  it should "convert to Rational" in {
    // NOTE: this is rather artificial
    val pi = BigDecimal(math.Pi)
    val r = Rational(pi)
    r.toDouble shouldBe math.Pi
  }

  behavior of "toBigInt"
  it should "work for Long.MaxValue" in {
    val r = Rational(Long.MaxValue)
    r.toBigInt shouldBe BigInt(Long.MaxValue)
  }

  behavior of "toLong"
  it should "work for Long.MaxValue" in {
    val r = Rational(Long.MaxValue)
    r.toLong shouldBe Long.MaxValue
  }

  behavior of "toBigDecimal"
  it should "work for pi" in {
    val pi = BigDecimal(Math.PI)
    val r = Rational(pi)
    r.toBigDecimal shouldBe pi
    r.toDouble shouldBe Math.PI +- 1E-15
  }

  behavior of "equals"
  it should "equate 0 and zero" in {
    Rational(0) shouldBe Rational.zero
    Rational.zero shouldBe Symbol("zero")
  }
  it should "be whole" in {
    Rational.zero shouldBe Symbol("whole")
  }
  it should "equal 0" in {
    Rational.zero.toInt shouldBe 0
  }
  it should "equal infinity when inverted" in {
    Rational.zero.invert shouldBe Symbol("infinity")
  }
  it should "equal BigDecimal.ZERO" in {
    Rational.zero.toBigDecimal shouldBe BigDecimal(0)
  }
  it should "equal r when added to r" in {
    val r = Rational(22, 7) // we could choose anything here
    (Rational.zero + r) shouldBe r
  }
  it should "equal infinity when r-interpolator has 0 denominator" in {
    r"1/0" shouldBe Symbol("infinity")
  }

  behavior of "+"
  it should "return 4 for 2+2" in {
    val r1 = Rational(2)
    val r2 = Rational(2)
    val r = r1 + r2
    r shouldBe Rational(4, 1)
  }
  it should "return result for 0+-236274181" in {
    val n1 = BigInt(0)
    val d1 = -1L
    val n2 = BigInt(-2362741811L)
    val d2 = 1L
    val r = Rational(n1, d1) + Rational(n2, d2)
    r shouldBe Rational(BigInt(-2362741811L), 1)
  }
  it should "add 0 to large number" in {
    val r1 = Rational(0)
    val r2 = Rational(BigInt("18050442446843353054"))
    val r = r1 + r2
    Rational.hasCorrectRatio(r, BigInt("18050442446843353054"), 1L) shouldBe true
  }
  it should "add 1 to large number" in {
    val r1 = Rational(1)
    val r2 = Rational(BigInt("18050442446843353054"), 2)
    val r = r1 + r2
    Rational.hasCorrectRatio(r, 2 + BigInt("18050442446843353054"), 2L) shouldBe true
  }
  it should "add large number to smaller one" in {
    val r1 = Rational(BigInt("-9223372036854775808"), 1)
    val r2 = Rational(BigInt("816512980"), -1)
    val r = r1 + r2
    Rational.hasCorrectRatio(r, 816512980 - BigInt("-9223372036854775808"), -1) shouldBe true
  }

  behavior of "negate"
  it should "work for -1" in {
    val r = Rational(-1)
    r.negate shouldBe Rational.one
  }

  behavior of "implicit conversion"
  it should "work for 0" in {
    val r: Rational = BigInt(0)
    r shouldBe Rational.zero
  }

  behavior of "1/2"
  it should "be OK" in {
    Rational.half * 2 shouldBe Rational.one
  }
  it should "equal half" in {
    Rational("1/2") shouldBe Rational.half
  }
  it should "be half of one" in {
    Rational.half * 2 shouldBe Rational.one
  }
  it should "be OK using r-interpolator" in {
    r"1/2" * 2 shouldBe Rational.one
  }
  it should "be OK using r-interpolator with variable" in {
    val denominator = 2
    r"1/$denominator" * denominator shouldBe Rational.one
  }

  behavior of "1"
  it should "be OK" in {
    Rational(1)
  }
  it should "be one" in {
    Rational(1) shouldBe Rational.one
  }
  it should "be positive" in {
    Rational.one.signum shouldBe 1
  }
  it should "be whole" in {
    Rational.one shouldBe Symbol("whole")
  }
  it should "be unity" in {
    Rational.one shouldBe Symbol("unity")
  }
  it should "equal 1" in {
    Rational.one.toInt shouldBe 1
  }
  it should "not equal infinity when inverted" in {
    Rational.one.invert should not be Symbol("infinity")
  }
  it should "equal itself when inverted" in {
    Rational.one.invert shouldBe Rational.one
  }
  it should "equal BigDecimal.one" in {
    Rational.one.toBigDecimal shouldBe BigDecimal(1)
  }
  it should "equal r when multiplied by r" in {
    val r = Rational(22, 7) // we could choose anything here
    (Rational.one * r) shouldBe r
  }
  it should "be -1 when negated" in {
    val r = Rational.one
    -r shouldBe (Rational.one * -1)
    r.signum shouldBe 1
  }

  behavior of "power"
  it should "work" in {
    val ten = Rational.ten
    ten.power(2) shouldBe Rational(100)
    ten.power(10) shouldBe Rational(10000000000L)
    ten.power(0) shouldBe Rational.one
    ten.power(-1) shouldBe ten.invert
  }

  behavior of "exponent"
  it should "work for 2" in {
    val hundred = Rational.exponent(2)
    hundred shouldBe Rational(100)
  }
  it should "work for Avagadro" in {
    val r = Rational(6.02214076)
    val avagadro = r.applyExponent(23)
    avagadro shouldBe Rational("6.02214076E23")
  }

  behavior of "10"
  it should "be OK" in {
    Rational(10)
  }
  it should "be ten" in {
    Rational(10) shouldBe Rational.ten
  }
  it should "be whole" in {
    Rational.ten shouldBe Symbol("whole")
  }
  it should "not be zero" in {
    Rational.ten should not be Symbol("zero")
  }
  it should "equal 10" in {
    Rational.ten.toInt shouldBe 10
  }
  it should "equal 5*2" in {
    (Rational.ten / 2) shouldBe Rational(5)
  }
  it should "equal 10*1" in {
    (Rational.ten / 10) shouldBe Rational.one
  }
  it should "equal BigDecimal(10)" in {
    Rational.ten.toBigDecimal shouldBe BigDecimal(10)
  }
  it should "equal a million when raised to 6th power" in {
    (Rational.ten ^ 6) shouldBe Rational(1000000)
  }

  behavior of "2/3"
  it should "be OK" in {
    Rational(2, 3)
  }
  it should "equal -1/3 when added to -1" in {
    Rational(2, 3) - Rational.one shouldBe Rational(-1, 3)
  }
  it should "be less than 1" in {
    Rational(2, 3).compare(Rational.one) shouldBe (-1)
  }
  it should "not be whole" in {
    Rational(2, 3) should not be Symbol("whole")
  }
  it should "equal 2 when multiplied by 3" in {
    (Rational(2, 3) * 3 toInt) shouldBe 2
  }
  it should "equal 3/2 when inverted" in {
    Rational(2, 3).invert shouldBe Rational(3, 2)
  }
  it should "equal 5/3 when added to 1" in {
    (Rational.one + Rational(2, 3)) shouldBe Rational(5, 3)
  }
  it should "equal 4/9 when multiplied by itself" in {
    val r = Rational(2, 3)
    (r * r) shouldBe Rational(4, 9)
  }
  it should "equal 4/9 when squared" in {
    (Rational(2, 3) ^ 2) shouldBe Rational(4, 9)
  }
  it should "barf when toInt invoked" in {
    an[RationalException] should be thrownBy Rational(2, 3).toInt
    val thrown = the[Exception] thrownBy Rational(2, 3).toInt
    thrown.getMessage should equal("toBigInt: 2/3 is not whole")
  }

  behavior of "2/4"
  it should "not be OK" in {
    val thrown = the[IllegalArgumentException] thrownBy new Rational(2, 4)
    thrown.getMessage should equal("requirement failed: Rational(2,4): arguments have common factor: 2")
  }
  it should "not OK via apply" in {
    Rational(2, 4) shouldBe Rational.half
  }

  behavior of "Floating Point Problem"
  it should "be OK" in {
    val x = Rational(1, 10) + Rational(2, 10)
    val y = x * 10 / 3
    y shouldBe Symbol("unity")
  }

  behavior of "toString"
  it should "be decimal when exact" in {
    val r = Rational(1, 2)
    r.toString() shouldBe "0.5"
  }
  it should "be rational when not exact: 2/3" in {
    val r = Rational(2, 3)
    r.toString() shouldBe "2/3"
  }
  it should "be decimal when not exact: pi" in {
    val pi = Rational(BigDecimal(math.Pi))
    pi.toString() shouldBe "3.141592653589793"
  }
  it should "work for NaN" in {
    Rational.NaN.toString shouldBe "NaN"
  }
  it should "work for Infinity" in {
    Rational.infinity.toString shouldBe "+ve infinity"
  }
  it should "work for negative Infinity" in {
    Rational.infinity.negate.toString shouldBe "-ve infinity"
  }

  behavior of "Rational(String)"
  it should "work for 0.1" in {
    val r = Rational("0.1")
    r shouldBe Rational(1, 10)
  }
  it should "work for 1.0e6" in {
    val r = Rational("1.0e6")
    r shouldBe Rational(10).power(6)
  }
  it should "convert Avagadro's number" in {
    val r = Rational("6.02214076E23")
    r.toBigInt shouldBe BigInt("602214076000000000000000")
  }

  behavior of "parse(String)"
  it should "work for 0.1" in {
    val r = Rational.parse("0.1")
    r shouldBe Success(Rational(1, 10))
  }
  it should "work for 1.0e6" in {
    val r = Rational.parse("1.0e6")
    r shouldBe Success(Rational(10).power(6))
  }
  it should "work for 15699511928844194920/4294967295" in {
    val r = Rational.parse("15699511928844194920/4294967295")
    r should matchPattern { case Success(Rational(_, _)) => }
  }
  it should "work for 15699511928844194920" in {
    val r = Rational.parse("15699511928844194920")
    r should matchPattern { case Success(Rational(b, Rational.bigOne)) if b == BigInt("15699511928844194920") => }
  }
  it should "work for -15699511928844194920/4294967295" in {
    val r = Rational.parse("-15699511928844194920/4294967295")
    r should matchPattern { case Success(Rational(_, _)) => }
  }
  it should "work for -15699511928844194920/-4294967295" in {
    val r = Rational.parse("-15699511928844194920/-4294967295")
    r should matchPattern { case Success(Rational(_, _)) => }
  }
  it should "fail for x" in {
    val r = Rational.parse("x")
    r should matchPattern { case Failure(_) => }
  }

  behavior of "edu/neu/coe/csye7200/sorting"
  it should "work" in {
    val r = List(Rational(1, 2), Rational(2, 3), Rational(1, 3))
    val x = r.sorted
    x.head shouldBe Rational(1, 3)
    x.tail.head shouldBe Rational(1, 2)
    x.tail.tail.head shouldBe Rational(2, 3)
  }

  behavior of "r-interpolator"
  it should "work for -1/0" in {
    val r = r"-1/0"
    r.isInfinity shouldBe true
    r shouldBe Rational(-1, 0)
  }
  it should "work for 1/-2147483648" in {
    val r = r"1/-2147483648"
    r.signum shouldBe -1
  }
  it should "work for -1/-2147483648" in {
    val r = r"-1/-2147483648"
    r.signum shouldBe 1
  }

  // Test Private Methods...
  behavior of "narrow(BigInt)"
  it should "work for Int.MaxValue" in {
    val b = BigInt(Int.MaxValue)
    val decorateNarrow = PrivateMethod[Try[BigInt]](Symbol("narrow"))
    val z: Try[BigInt] = Rational invokePrivate decorateNarrow(b, BigInt(Int.MaxValue))
    z should matchPattern { case Success(x) if x == Int.MaxValue => }
  }
  it should "not work for Int.MaxValue+1" in {
    val b = BigInt(Int.MaxValue) + 1
    val decorateNarrow = PrivateMethod[Try[BigInt]](Symbol("narrow"))
    val z: Try[BigInt] = Rational invokePrivate decorateNarrow(b, BigInt(Int.MaxValue))
    z should matchPattern { case Failure(_) => }
  }

  behavior of "narrow(Rational)"
  it should "work for Int.MaxValue" in {
    val r = Rational(Int.MaxValue)
    val decorateNarrow = PrivateMethod[Try[BigInt]](Symbol("narrow"))
    val z: Try[BigInt] = Rational invokePrivate decorateNarrow(r, BigInt(Int.MaxValue))
    z should matchPattern { case Success(x) if x == Int.MaxValue => }
  }
  it should "work for Int.MaxValue+1" in {
    val r = Rational(Int.MaxValue) + 1
    val decorateNarrow = PrivateMethod[Try[BigInt]](Symbol("narrow"))
    val z: Try[BigInt] = Rational invokePrivate decorateNarrow(r, BigInt(Int.MaxValue))
    z should matchPattern { case Failure(_) => }
  }

  behavior of "normalize"
  it should "work for 0,0" in {
    val decorateNormalize = PrivateMethod[Rational](Symbol("normalize"))
    val z = Rational invokePrivate decorateNormalize(Rational.bigZero, Rational.bigZero)
    z should matchPattern { case Rational(x, y) if x == Rational.bigZero && y == 0L => }
  }
  it should "work for 1,0" in {
    val decorateNormalize = PrivateMethod[Rational](Symbol("normalize"))
    val z = Rational invokePrivate decorateNormalize(Rational.bigOne, Rational.bigZero)
    z should matchPattern { case Rational(x, y) if x == Rational.bigOne && y == 0L => }
  }
  it should "work for 1,1" in {
    val decorateNormalize = PrivateMethod[Rational](Symbol("normalize"))
    val z = Rational invokePrivate decorateNormalize(Rational.bigOne, Rational.bigOne)
    z should matchPattern { case Rational(x, y) if x == Rational.bigOne && y == 1L => }
  }
  it should "work for 2,2" in {
    val decorateNormalize = PrivateMethod[Rational](Symbol("normalize"))
    val z = Rational invokePrivate decorateNormalize(Rational.bigOne * 2, BigInt(2))
    z should matchPattern { case Rational(x, y) if x == Rational.bigOne && y == 1L => }
  }
  it should "work for 3,5" in {
    val decorateNormalize = PrivateMethod[Rational](Symbol("normalize"))
    val z = Rational invokePrivate decorateNormalize(Rational.bigOne * 3, BigInt(5))
    z should matchPattern { case Rational(x, y) if x == BigInt(3) && y == 5L => }
  }

  behavior of "gcd"
  it should "work for 0,0" in {
    val decorateGcd = PrivateMethod[BigInt](Symbol("gcd"))
    val z = Rational invokePrivate decorateGcd(Rational.bigZero, Rational.bigZero)
    z should matchPattern { case Rational.bigZero => }
  }
  it should "work for 1,0" in {
    val decorateGcd = PrivateMethod[BigInt](Symbol("gcd"))
    val z = Rational invokePrivate decorateGcd(Rational.bigOne, Rational.bigZero)
    z should matchPattern { case Rational.bigOne => }
  }
  it should "work for 1,1" in {
    val decorateGcd = PrivateMethod[BigInt](Symbol("gcd"))
    val z = Rational invokePrivate decorateGcd(Rational.bigOne, Rational.bigOne)
    z should matchPattern { case Rational.bigOne => }
  }
  it should "work for 2,2" in {
    val decorateGcd = PrivateMethod[BigInt](Symbol("gcd"))
    val z = Rational invokePrivate decorateGcd(Rational.bigOne * 2, Rational.bigOne * 2)
    z should matchPattern { case b: BigInt if b.toInt == 2 => }
  }
  it should "work for 3,5" in {
    val decorateGcd = PrivateMethod[BigInt](Symbol("gcd"))
    val z = Rational invokePrivate decorateGcd(Rational.bigOne * 3, Rational.bigOne * 5)
    z should matchPattern { case Rational.bigOne => }
  }

  behavior of "compare"
  it should "work for 0,0" in {
    val decorateCompare = PrivateMethod[Int](Symbol("compare"))
    val z = Rational invokePrivate decorateCompare(Rational.zero, Rational.zero)
    z should matchPattern { case 0 => }
  }
  it should "work for 0,1" in {
    val decorateCompare = PrivateMethod[Int](Symbol("compare"))
    val z = Rational invokePrivate decorateCompare(Rational.zero, Rational.one)
    z should matchPattern { case -1 => }
  }
  it should "work for 1,0" in {
    val decorateCompare = PrivateMethod[Int](Symbol("compare"))
    val z = Rational invokePrivate decorateCompare(Rational.one, Rational.zero)
    z should matchPattern { case 1 => }
  }
  it should "work for 1,1" in {
    val decorateCompare = PrivateMethod[Int](Symbol("compare"))
    val z = Rational invokePrivate decorateCompare(Rational.one, Rational.one)
    z should matchPattern { case 0 => }
  }
  it should "work for inf,inf" in {
    val decorateCompare = PrivateMethod[Int](Symbol("compare"))
    val z = Rational invokePrivate decorateCompare(Rational.infinity, Rational.infinity)
    z should matchPattern { case 0 => }
  }
  it should "work for -inf,inf" in {
    val decorateCompare = PrivateMethod[Int](Symbol("compare"))
    val z = Rational invokePrivate decorateCompare(Rational.infinity.unary_-, Rational.infinity)
    z should matchPattern { case 0 => }
  }

  behavior of "mediant"
  it should "work" in {
    Rational.zero mediant Rational.one shouldBe Rational.half
    Rational.one mediant Rational.zero shouldBe Rational.half
    Rational.zero mediant Rational.half shouldBe Rational(1, 3)
    Rational.half mediant Rational.zero shouldBe Rational(1, 3)
    Rational(1, 3) mediant Rational(3, 2) shouldBe Rational(4, 5)
    Rational(0, 1) mediant Rational.infinity shouldBe Rational.NaN
    Rational.infinity mediant Rational.zero shouldBe Rational.NaN
  }

  behavior of "approximate"
  it should "work" in {
    Rational.approximate(1.0 / 2) shouldBe Rational.half
    Rational.approximate(1.0 / 3) shouldBe Rational(1, 3)
    Rational.approximate(3.0 / 4) shouldBe Rational(3, 4)
  }

  it should "fail" in {
    a[IllegalArgumentException] should be thrownBy Rational.approximate(Math.PI)
    a[IllegalArgumentException] should be thrownBy Rational.approximate(-0.5)
  }

  behavior of "approximateAny"
  it should "work for specific epsilon" in {
    implicit val epsilon: Tolerance = Tolerance(1E-7)
    Rational.approximateAny(Math.PI) shouldBe Rational(75948, 24175)
  }

  behavior of "doubleToRational"
  it should "work" in {
    Rational.doubleToRational(1.0 / 2) shouldBe Rational.half
    Rational.doubleToRational(-1.0 / 3) shouldBe Rational(-1, 3)
    Rational.doubleToRational(-5.0 / 4) shouldBe Rational(-5, 4)
    Rational.doubleToRational(Math.PI).toDouble shouldBe Math.PI +- 1E-15
    Rational.doubleToRational(6.02214076E23).toDouble shouldBe 6.02214076E23 +- 1E9
  }
}

object RationalSpec {


}
