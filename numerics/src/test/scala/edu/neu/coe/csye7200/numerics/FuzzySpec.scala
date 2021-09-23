package edu.neu.coe.csye7200.numerics

import org.scalatest.flatspec
import org.scalatest.matchers.should

import scala.util.Success

/**
  * @author scalaprof
  */
class FuzzySpec extends flatspec.AnyFlatSpec with should.Matchers {

  def checkDouble(actual: Double, expected: Double): Unit = {
    assert(math.abs(actual - expected) < 1E7)
  }

  "Gaussian" should "sum with self" in {
    val dist = Gaussian(2, 2)
    val dist2 = dist + dist
    dist2 should matchPattern {
      case Gaussian(4, _) =>
    }
    dist2 match {
      case Gaussian(_, s) => checkDouble(s, 2 * math.sqrt(2))
      case _ => fail
    }
  }
  it should "not multiply with self" in {
    val dist = Gaussian(2, 2)
    an[UnsupportedOperationException] should be thrownBy dist * dist
  }

  "Bounded" should "sum with self" in {
    val dist = Bounded(2, 0.1)
    val dist2 = dist + dist
    dist2 should matchPattern {
      case Bounded(4, _) =>
    }
    dist2 match {
      case Bounded(_, s) => checkDouble(s, 0.2)
      case _ => fail
    }
  }
  it should "multiply with self" in {
    val dist = Bounded(2, 0.1)
    val dist2 = dist * dist
    dist2 should matchPattern {
      case Bounded(4, _) =>
    }
    dist2 match {
      case Bounded(_, s) => checkDouble(s, 0.4)
      case _ => fail
    }
  }
  it should "implement negate" in {
    val dist = Bounded(2, 0.1)
    val dist2 = dist.negate
    dist2 should matchPattern {
      case Bounded(-2, 0.1) =>
    }
  }
  it should "implement inverse" in {
    val dist = Bounded(2, 0.1)
    val dist2 = 1 / dist
    dist2 should matchPattern {
      case Bounded(0.5, 0.025) =>
    }
  }
  it should "implement power(Int)" in {
    val dist = Bounded(2, 0.1)
    val dist2 = dist.power(2)
    dist2 should matchPattern {
      case Bounded(4, _) =>
    }
    dist2 match {
      case Bounded(_, s) => checkDouble(s, 2 * 2 * 0.1)
      case _ => fail
    }
  }
  it should "implement power(Double)" in {
    val dist = Bounded(2, 0.1)
    val dist2 = dist.power(2.0)
    dist2 should matchPattern {
      case Bounded(4, _) =>
    }
    dist2 match {
      case Bounded(_, s) => checkDouble(s, 2 * 2 * 0.1)
      case _ => fail
    }
  }
  it should "implement power(Fuzzy)" in {
    val dist = Bounded(2, 0.1)
    val dist2 = dist.power(dist)
    dist2 should matchPattern {
      case Bounded(4, _) =>
    }
    dist2 match {
      case Bounded(_, s) => checkDouble(s, (math.log(2) + 1) * 2 * 2 * 0.1)
      case _ => fail
    }
  }
  "fuzzy(String)" should "parse 1" in {
    Fuzzy.parse("1") should matchPattern { case Success(Exact(1)) => }
  }
  it should "parse 6.67408(31)E−11" in {
    Fuzzy.parse("6.67408(31)E-11") match {
      case Success(Gaussian(m, s)) => assert(math.abs(m - 6.67408E-11) < 1E-5); assert(math.abs(s - 3.1E-14) < 1E-10)
      case f@_ => fail(s"should be Gaussian but is $f")
    }
  }
  it should "parse 3.1415927(01)" in {
    Fuzzy.parse("3.1415927(01)") match {
      case Success(Gaussian(m, s)) => assert(math.abs(m - 3.1415927) < 1E-5); assert(math.abs(s - 0.000001) < 1E-10)
      case f@_ => fail(s"should be Gaussian but is $f")
    }
  }
  it should "parse 3.1415927" in {
    Fuzzy.parse("3.1415927") match {
      case Success(Exact(x)) => assert(math.abs(x - 3.1415927) < 1E-5)
      case f@_ => fail(s"should be Exact but is $f")
    }
  }

}
