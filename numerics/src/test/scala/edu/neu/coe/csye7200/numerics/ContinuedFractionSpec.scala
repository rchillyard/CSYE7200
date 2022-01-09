package edu.neu.coe.csye7200.numerics

import edu.neu.coe.csye7200.numerics.ConFrac.LongLazyListFrom
import edu.neu.coe.csye7200.numerics.ContinuedFraction.{Hurwitz, fPiBy4Leibniz}
import edu.neu.coe.csye7200.numerics.Rational.RationalHelper
import org.scalatest.flatspec
import org.scalatest.matchers.should

class ContinuedFractionSpec extends flatspec.AnyFlatSpec with should.Matchers {

  val goldenRatio: Double = (1 + math.sqrt(5)) / 2

  behavior of "ConFrac.take"

  it should "create singleton simple ConFrac" in {
    val one: LazyList[Long] = LongLazyListFrom(1).take(1)
    val cf = ConFrac.simple(one)
    cf.b shouldBe 1
    cf.tailOption shouldBe None
  }

  it should "create doubleton simple ConFrac" in {
    val one = LongLazyListFrom(1).take(2)
    val cf = ConFrac.simple(one)
    cf.b shouldBe 1
    val co = cf.tailOption
    co.get.a shouldBe 1
    co.get.c.b shouldBe 2
    co.get.c.tailOption shouldBe None
  }

  behavior of "ConFrac.takeWhile"

  it should "phi ConFrac.simple precise to 1E-6" in {
    val one = LazyList.continually(1L)
    val target = ConFrac.simple(one)
    val epsilon = 1E-6

    def unprecise(r: Rational): Boolean = 1.0 / r.d.toDouble / r.d.toDouble / math.sqrt(5) > epsilon

    val cf = target.takeWhile(unprecise)
    cf.reverseCoefficients.length shouldBe 16
    cf.toRational.toDouble shouldBe goldenRatio +- epsilon
  }

  it should "fail to evaluate phi ConFrac precise to 1E-6" in {
    val one = LazyList.continually(1L)
    val target = ConFrac.simple(one).take(5)
    val epsilon = 1E-6

    def unprecise(r: Rational): Boolean = 1.0 / r.d.toDouble / r.d.toDouble / math.sqrt(5) > epsilon

    val cf = target.takeWhile(unprecise)
    a[ConFracException] should be thrownBy cf.reverseCoefficients
  }

  behavior of "ConFrac.convergents"

  val lFib: LazyList[Long] = 1L #:: lFib.scanLeft(1L)(_ + _)
  // NOTE: these values are: 1, 2, 3/2, 5/3, 8/5, 13/8, 21/13, 34/21, 55/34, 89/55, 144/89, etc.
  val convergentsPhi: LazyList[Rational] = Pair.zip(lFib, lFib.tail) map (p => p.toRational)

  def tupleMatch(t: (Any, Any)): Boolean = t._1 == t._2

  it should "get convergents from simple ConFrac" in {
    val one: LazyList[Long] = LazyList.continually(1L)
    val target = ConFrac.simple(one)
    target.convergents.take(5).zip(convergentsPhi).forall(tupleMatch) shouldBe true
  }

  it should "get convergents from simple finite expansion for pi" in {
    val target = ConFrac.PiSimple
    val rs = target.convergents.toList
    rs.head shouldBe Rational(3)
    rs(1) shouldBe r"22/7"
    rs(2) shouldBe r"333/106"
    rs(3) shouldBe r"355/113"
    rs(4) shouldBe r"103993/33102"
  }

  it should "get convergents for e" in {
    val target = ConFrac.E
    val rs = target.convergents.take(5).toList
    rs.head shouldBe Rational.two
    rs(1) shouldBe Rational(3)
    rs(2) shouldBe r"8/3"
    rs(3) shouldBe r"11/4"
    rs(4) shouldBe r"19/7"
  }

  it should "get convergents for root 2" in {
    val target = ContinuedFraction.root2
    val rs = target.convergents.take(11).toList
    rs.head shouldBe Rational.one
    rs(1) shouldBe r"3/2"
    rs(2) shouldBe r"7/5"
    rs(3) shouldBe r"17/12"
    rs(4) shouldBe r"41/29"
    rs(5) shouldBe r"99/70"
    rs(6) shouldBe r"239/169"
    rs(7) shouldBe r"577/408"
    rs(8) shouldBe r"1393/985"
    rs(9) shouldBe r"3363/2378"
    rs(10) shouldBe r"8119/5741"
  }

  behavior of "ConFrac.coefficients"

  it should "work on finite simple ConFrac" in {
    val one = LongLazyListFrom(1).take(2)
    val cf = ConFrac.simple(one)
    val xs: LazyList[Pair] = cf.coefficients
    xs.head shouldBe Pair(1, 0)
    xs.tail.head shouldBe Pair(2)
  }

  it should "work on very short finite simple ConFrac" in {
    val one = LongLazyListFrom(1)
    val cf = ConFrac.simple(one).take(0)
    val xs: LazyList[Pair] = cf.coefficients
    xs.head shouldBe Pair(1, 0)
    xs.tail shouldBe LazyList()
  }

  it should "work on infinite simple ConFrac" in {
    val one = LongLazyListFrom(1)
    val cf = ConFrac.simple(one)
    val xs: LazyList[Pair] = cf.coefficients
    xs.head shouldBe Pair(1, 0)
    xs.tail.head shouldBe Pair(2)
    xs.tail.tail.head shouldBe Pair(3)
  }

  it should "work on finite general ConFrac" in {
    val pairs: LazyList[Pair] = ContinuedFraction.fPiBy4Leibniz
    val cf = ConFrac(pairs)
    val xs: LazyList[Pair] = cf.coefficients
    xs.head shouldBe Pair(1, 0)
    xs.tail.head shouldBe Pair(2, 1)
    xs.tail.tail.head shouldBe Pair(2, 9)
  }

  behavior of "ConFrac.reverseCoefficients"

  it should "work on finite simple ConFrac" in {
    val one = LongLazyListFrom(1).take(2)
    val cf = ConFrac.simple(one)
    val xs: LazyList[Pair] = cf.reverseCoefficients
    xs.head shouldBe Pair(2)
    xs.tail.head shouldBe Pair(1)
  }

  behavior of "ConFrac.toRational"

  it should "implement simple toRational" in {
    // NOTE: these tests approximate the Golden Ratio phi
    val target = ConFrac.simple(LazyList.continually(1))
    target.take(0).toRational shouldBe convergentsPhi.head
    target.take(1).toRational shouldBe convergentsPhi(1)
    target.take(2).toRational shouldBe convergentsPhi(2)
    target.take(3).toRational shouldBe convergentsPhi(3)
    target.take(4).toRational shouldBe convergentsPhi(4)
    target.take(5).toRational shouldBe convergentsPhi(5)
    target.take(6).toRational shouldBe convergentsPhi(6)
    target.take(7).toRational shouldBe convergentsPhi(7)
    target.take(8).toRational shouldBe convergentsPhi(8)
    target.take(9).toRational shouldBe convergentsPhi(9)
    target.take(10).toRational shouldBe convergentsPhi(10)

  }

  it should "implement simple toRationalOption(Rational)" in {
    val cf = ConFrac.simple(LazyList.continually(1))
    cf.toRationalOption(Rational(10).invert).get shouldBe convergentsPhi(3)
    cf.toRationalOption(Rational(25).invert).get shouldBe convergentsPhi(4)
    cf.toRationalOption(Rational(50).invert).get shouldBe convergentsPhi(5)
    cf.toRationalOption(Rational(100).invert).get shouldBe convergentsPhi(6)

  }

  it should "implement simple finite toRationalOption(Rational)" in {
    val cf = ConFrac.simple(LazyList.continually(1)).take(5)
    cf.toRationalOption(Rational(10).invert).get shouldBe convergentsPhi(3)
    cf.toRationalOption(Rational(25).invert).get shouldBe convergentsPhi(4)
    cf.toRationalOption(Rational(50).invert) should matchPattern { case None => }
  }

  it should "implement simple toRational(Double)" in {
    val one: LazyList[Long] = LongLazyListFrom(1)
    val cf: ConFrac = ConFrac.simple(one)

    import edu.neu.coe.csye7200.numerics.Rational.RationalHelper
    cf.toRational(0.05)(Hurwitz) shouldBe Some(r"10/7")
    cf.toRational(0.01)(Hurwitz) shouldBe Some(r"43/30")
    cf.toRational(0.005)(Hurwitz) shouldBe Some(r"43/30")
    cf.toRational(0.001)(Hurwitz) shouldBe Some(r"225/157")
    cf.toRational(0.0001)(Hurwitz) shouldBe Some(r"1393/972")
    cf.toRational(0.000005)(Hurwitz) shouldBe Some(r"9976/6961")
    cf.toRational(0.0000001)(Hurwitz) shouldBe Some(r"81201/56660")
    cf.toRational(0.000000002)(Hurwitz) shouldBe Some(r"740785/516901")
    cf.toRational(0.00000000005)(Hurwitz) shouldBe Some(r"7489051/5225670")
  }

  it should "define implement coefficients for FourOverPiLeibniz" in {
    val target: ConFrac = ConFrac(fPiBy4Leibniz)
    val z: LazyList[Pair] = target.coefficients
    z.head shouldBe Pair(1, 0)
    z.tail.head shouldBe Pair(2, 1)
  }

  behavior of "ConFrac.phi"

  it should "toDouble" in {
    ConFrac.phi.toDouble(1E-9, Hurwitz).get shouldBe goldenRatio +- 1E-4
  }

  it should "implement toDouble(Double)" in {
    def checkValue(epsilon: Double): Unit = {
      import Ordering.Double.IeeeOrdering
      // NOTE: we need a fairly large power because phi converges so slowly.
      val maybeDouble = ConFrac.phi.toDouble(epsilon, Hurwitz)
      maybeDouble match {
        case Some(x) => math.abs(x - goldenRatio) should be < epsilon
        case None => fail("unable to get Double from phi")
      }
    }

    checkValue(0.1)
    checkValue(0.01)
    checkValue(0.001)
    checkValue(0.0001)
    checkValue(0.00001)
    checkValue(0.000001)
    checkValue(0.0000001)
    checkValue(0.00000001)
    checkValue(0.000000001)
  }

  behavior of "ConFrac.e"

  it should "define ConFrac" in {
    val z: ConFrac = ConFrac.E.take(20)
    val q: Rational = z.toRational
    q shouldBe r"410105312/150869313"
    q.toDouble shouldBe math.E +- 1E-10
  }

  behavior of "ConFrac.PiSimple"

  it should "define ConFrac" in {
    val z: ConFrac = ConFrac.PiSimple
    val q: Rational = z.toRational
    q.toDouble shouldBe math.Pi +- 1E-10
  }

  behavior of "ConFrac.root2"

  it should "define ConFrac" in {
    val z: ConFrac = ConFrac.root2.take(20)
    val q: Rational = z.toRational
    q.toDouble shouldBe math.sqrt(2) +- 1E-10
  }

  it should "solve the Strand magazine puzzle" in {
    // NOTE: the solutions are the values of Y and X (numerator and denominator), e.g. 3, 2 or 17, 12
    // Now, the umber of houses on the street y is such that Y = 2y + 1
    // And the house number x is such that X = 2x
    // However, only the results from the odd numbers of terms are the solution to the Strand puzzle by Henry Dudeney,
    // which obeys the Pell equation = Y^2 - 2 X^2 = 1 (see https://en.wikipedia.org/wiki/Pell%27s_equation).
    // Incidentally, the even numbers of terms result in solutions to the complementary Pell equation: Y^2 - 2 X^2 = - 1
    // Obviously, if we solve Y^2 - = 2 X^2, we get the square root of 2, but this is an irrational number, not a rational number.
    val cf: ConFrac = ConFrac.root2
    cf.take(1).toRational shouldBe r"3/2" // NOTE: house # 1 on street of 1 houses
    cf.take(3).toRational shouldBe r"17/12" // NOTE: house # 6 on street of 8 houses
    cf.take(5).toRational shouldBe r"99/70" // NOTE: house # 35 on street of 49 houses
    cf.take(7).toRational shouldBe r"577/408" // NOTE: house # 204 on street of 288 houses
    cf.take(9).toRational shouldBe r"3363/2378" // NOTE: house # 1189 on street of 1681 houses
  }

  behavior of "ConFrac.root3"

  it should "define ConFrac" in {
    val z: ConFrac = ConFrac.root3.take(20)
    val q: Rational = z.toRational
    q.toDouble shouldBe math.sqrt(3) +- 1E-10
  }

  behavior of "ConFrac.root19"

  val root19: Double = math.sqrt(19)

  it should "define ConFrac" in {
    val z: ConFrac = ConFrac.root19.take(20)
    val q: Rational = z.toRational
    q shouldBe r"512445947/117563163"
    (q * q).toDouble shouldBe 19.0 +- 1E-10
  }

  it should "toDouble" in {
    val maybeDouble = ConFrac.root19.toDouble(1E-6, Hurwitz)
    maybeDouble.nonEmpty shouldBe true
    // TODO: find out why we don't get precision of 1E-6
    maybeDouble.get shouldBe root19 +- 1.0E-4
  }

  it should "implement toDouble(Double)" in {
    def checkValue(epsilon: Double): Unit = {
      // TODO: find out why we need this divisor of 6
      ConFrac.root19.toDouble(epsilon / 6, Hurwitz).foreach {
        x => math.abs(x - root19) should be < epsilon
      }
    }

    checkValue(0.1)
    checkValue(0.01)
    checkValue(0.001)
    checkValue(0.0001)
    checkValue(0.00001)
    checkValue(0.000001)
    checkValue(0.0000001)
    checkValue(0.00000001)
    checkValue(0.000000001)
  }

  behavior of "ConFrac.unapply"

  it should "work" in {
    val target: ConFrac = ConFrac.root19
    ConFrac.unapply(target) should matchPattern { case Some((4, Some(CF(1, _)))) => }
  }

  behavior of "ContinuedFraction.prefix"

  it should "create singleton ContinuedFraction" in {
    val target = ContinuedFraction.createInfinite(Pair(1)).prefix(0)
    target.cf.b shouldBe 1
    target.cf.tailOption shouldBe None
  }

  behavior of "ContinuedFraction.takeWhile"

  it should "phi ContinuedFraction precise to 1E-6" in {
    val target = ContinuedFraction.createInfinite(1)
    val epsilon = 1E-6

    def unprecise(r: Rational): Boolean = 1.0 / r.d.toDouble / r.d.toDouble / math.sqrt(5) > epsilon

    val cf = target.cf.takeWhile(unprecise)
    cf.reverseCoefficients.length shouldBe 16
    cf.toRational.toDouble shouldBe goldenRatio +- epsilon
  }

  behavior of "ContinuedFraction.toRational"

  it should "implement toRational" in {
    // NOTE: these tests approximate the Golden Ratio phi
    val target = ContinuedFraction.createInfinite(1)
    target.toRational(1) shouldBe Rational.two
    target.toRational(2) shouldBe r"3/2"
    target.toRational(3) shouldBe r"5/3"
    target.toRational(4) shouldBe r"8/5"
    target.toRational(5) shouldBe r"13/8"
    target.toRational(6) shouldBe r"21/13"
    target.toRational(7) shouldBe r"34/21"
    target.toRational(8) shouldBe r"55/34"
    target.toRational(9) shouldBe r"89/55"
    target.toRational(10) shouldBe r"144/89"
  }

  it should "implement toRational(Double)" in {
    val cf: ContinuedFraction = ContinuedFraction.createInfinite(1)

    import edu.neu.coe.csye7200.numerics.Rational.RationalHelper
    cf.toRational(0.05) shouldBe Some(r"5/3")
    cf.toRational(0.01) shouldBe Some(r"13/8")
    cf.toRational(0.005) shouldBe Some(r"21/13")
    cf.toRational(0.002) shouldBe Some(r"34/21")
    cf.toRational(0.001) shouldBe Some(r"55/34")
    cf.toRational(0.0001) shouldBe Some(r"144/89")
  }

  behavior of "ContinuedFraction.phi"

  it should "toDouble" in {
    ContinuedFraction.phi.toDouble(1E-9).get shouldBe goldenRatio +- 1E-4
  }

  it should "implement toDouble(Double)" in {
    def checkValue(epsilon: Double): Unit = {
      // NOTE: we need a fairly large power because phi converges so slowly.
      val maybeDouble = ContinuedFraction.phi.toDouble(epsilon)
      maybeDouble match {
        case Some(x) => math.abs(x - goldenRatio) should be < epsilon
        case None => fail("unable to get Double from phi")
      }
    }

    checkValue(0.1)
    checkValue(0.01)
    checkValue(0.001)
    checkValue(0.0001)
    checkValue(0.00001)
    checkValue(0.000001)
    checkValue(0.0000001)
    checkValue(0.00000001)
    checkValue(0.000000001)
  }

  behavior of "ContinuedFraction.e"

  it should "define ContinuedFraction" in {
    val z: ContinuedFraction = ContinuedFraction.E
    val q: Rational = z.toRational(20)
    q shouldBe r"410105312/150869313"
    q.toDouble shouldBe math.E +- 1E-10
  }

  behavior of "ContinuedFraction.pi"

  it should "define ContinuedFraction.PiSimple" in {
    val z: ContinuedFraction = ContinuedFraction.PiSimple
    implicit val threshold: Long = 10000000L
    z.convergents.foreach(r => println(r.render))
    val qo = z.toRational(1E-10)
    qo should matchPattern { case Some(_) => }
    qo.get.toDouble shouldBe math.Pi +- 1E-9
  }

  /**
    * NOTE: his representation of pi converges very slowly which is why we normally ignore it.
    */
  ignore should "define ContinuedFraction.FourOverPiLeibniz" in {
    val z: ContinuedFraction = ContinuedFraction.FourOverPiLeibniz
    val q: Rational = z.toRational(1000).invert
    q.toDouble shouldBe math.Pi / 4 +- 2.5E-4
  }

  it should "define ContinuedFraction.PiSomayaji" in {
    val z: ContinuedFraction = ContinuedFraction.PiSomayaji
    // NOTE: this representation of pi converges somewhat slowly.
    val q: Rational = z.toRational(75)
    q.toDouble shouldBe math.Pi +- 1E-6
  }

  it should "define ContinuedFraction.PiA" in {
    val z: ContinuedFraction = ContinuedFraction.PiA
    val q: Rational = z.toRational(21)
    q.toDouble shouldBe math.Pi +- 1E-15
  }

  behavior of "ContinuedFraction.root2"

  it should "define ContinuedFraction" in {
    val z: ContinuedFraction = ContinuedFraction.root2
    val q: Rational = z.toRational(20)
    q.toDouble shouldBe math.sqrt(2) +- 1E-10
  }

  behavior of "ContinuedFraction.root3"

  it should "define ContinuedFraction" in {
    val z: ContinuedFraction = ContinuedFraction.root3
    val q: Rational = z.toRational(20)
    q.toDouble shouldBe math.sqrt(3) +- 1E-10
  }

  behavior of "ContinuedFraction.root19"

  it should "define ContinuedFraction" in {
    val z: ContinuedFraction = ContinuedFraction.root19
    val q: Rational = z.toRational(20)
    q shouldBe r"512445947/117563163"
    (q * q).toDouble shouldBe 19.0 +- 1E-10
  }

  it should "toDouble" in {
    val maybeDouble = ContinuedFraction.root19.toDouble(1E-6)
    maybeDouble.nonEmpty shouldBe true
    maybeDouble.get shouldBe root19 +- 1.0E-4
  }

  it should "implement toDouble(Double)" in {
    def checkValue(epsilon: Double): Unit = {
      // TODO: find out why we need this divisor of 6
      ContinuedFraction.root19.toDouble(epsilon / 6).foreach {
        x => math.abs(x - root19) should be < epsilon
      }
    }

    checkValue(0.1)
    checkValue(0.01)
    checkValue(0.001)
    checkValue(0.0001)
    checkValue(0.00001)
    checkValue(0.000001)
    checkValue(0.0000001)
    checkValue(0.00000001)
    checkValue(0.000000001)
  }

  behavior of "Basel"
  it should "calculate pi" in {
    ??? // TO BE IMPLEMENTED
  }

}
