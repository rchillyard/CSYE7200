package edu.neu.coe.csye7200.parse

import edu.neu.coe.csye7200.numerics._
import org.scalatest.flatspec
import org.scalatest.matchers.should

/**
  * @author scalaprof
  */
class FuzzyParserSpec extends flatspec.AnyFlatSpec with should.Matchers {
  "integer" should "parse 1" in {
    val parser = new FuzzyParser
    val r = parser.parseAll(parser.wholeNumber, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get shouldBe "1"
  }
  it should "parse 6" in {
    val parser = new FuzzyParser
    val r = parser.parseAll(parser.wholeNumber, "6")
    r should matchPattern { case parser.Success(_, _) => }
    r.get shouldBe "6"
  }
  "fuzz" should "parse (42)" in {
    val parser = new FuzzyParser
    val r = parser.parseAll(parser.fuzz, "(42)")
    r should matchPattern { case parser.Success(_, _) => }
    r.get shouldBe "42"
  }
  it should "not parse 42" in {
    val parser = new FuzzyParser
    val r = parser.parseAll(parser.fuzz, "42")
    r should matchPattern { case parser.Failure(_, _) => }
  }
  "fraction" should "parse .42" in {
    val parser = new FuzzyParser
    val r = parser.parseAll(parser.fraction, ".42")
    r should matchPattern { case parser.Success(_, _) => }
    r.get shouldBe ".42"
  }
  "exponent" should "parse E-11" in {
    val parser = new FuzzyParser
    val r = parser.parseAll(parser.exponent, "E-11")
    r should matchPattern { case parser.Success(_, _) => }
    r.get shouldBe "-11"
  }
  it should "parse E11" in {
    val parser = new FuzzyParser
    val r = parser.parseAll(parser.exponent, "E11")
    r should matchPattern { case parser.Success(_, _) => }
    r.get shouldBe "11"
  }
  it should "parse e2" in {
    val parser = new FuzzyParser
    val r = parser.parseAll(parser.exponent, "e2")
    r should matchPattern { case parser.Success(_, _) => }
    r.get shouldBe "2"
  }
  "nominal" should "parse 1" in {
    val parser = new FuzzyParser
    val r = parser.parseAll(parser.nominal, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get._1 shouldBe "1"
    r.get._2 shouldBe None
    r.get._3 shouldBe None
  }
  it should "parse 6.67408" in {
    val parser = new FuzzyParser
    val r = parser.parseAll(parser.nominal, "6.67408")
    r should matchPattern { case parser.Success(_, _) => }
    r.get._1 shouldBe "6"
    r.get._2 shouldBe Some(".67408")
    r.get._3 shouldBe None
  }
  it should "parse 6.67408E-11" in {
    val parser = new FuzzyParser
    val r = parser.parseAll(parser.nominal, "6.67408E-11")
    r should matchPattern { case parser.Success(_, _) => }
    r.get._1 shouldBe "6"
    r.get._2 shouldBe Some(".67408")
    r.get._3 shouldBe Some("-11")
  }
  "fuzzyRep" should "parse 1" in {
    val parser = new FuzzyParser
    val r = parser.parseAll(parser.fuzzyRep, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get._1 shouldBe "1"
    r.get._2 shouldBe None
    r.get._3 shouldBe None
    r.get._4 shouldBe None
  }
  "partial parsing" should "parse 6.67408(31)E−11" in {
    val parser = new FuzzyParser
    val r = parser.parse(parser.wholeNumber, "6.67408(31)E-11")
    r should matchPattern { case parser.Success(_, _) => }
    r.get shouldBe "6"
    val next = r.next
    val r2 = parser.parse(parser.fraction, next)
    r2 should matchPattern { case parser.Success(_, _) => }
    r2.get shouldBe ".67408"
    val next2 = r2.next
    val r3 = parser.parse(parser.fuzz, next2)
    r3 should matchPattern { case parser.Success(_, _) => }
    r3.get shouldBe "31"
    val next3 = r3.next
    val r4 = parser.parse(parser.exponent, next3)
    val next4 = r4.next
    if (next4.offset < next4.source.length) println(s"ends looking at ${next3.source.charAt(next3.offset)}")
    r4 should matchPattern { case parser.Success(_, _) => }
    r4.get shouldBe "-11"
  }
  "fuzzyRep" should "parse 6.67408(31)E−11" in {
    val parser = new FuzzyParser
    val r = parser.parseAll(parser.fuzzyRep, "6.67408(31)E-11")
    r should matchPattern { case parser.Success(_, _) => }
    r.get._1 shouldBe "6"
    r.get._2 shouldBe Some(".67408")
    r.get._3 shouldBe Some("31")
    r.get._4 shouldBe Some("-11")
  }
  "fuzzy" should "be Exact(1)" in {
    val parser = new FuzzyParser
    val r = parser.parseAll(parser.fuzzy, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Exact(1) => }
  }
  it should "parse 6.67408(31)E−11" in {
    val parser = new FuzzyParser
    val r = parser.parseAll(parser.fuzzy, "6.67408(31)E-11")
    r should matchPattern { case parser.Success(_, _) => }
    r.get match {
      case Gaussian(m, s) => assert(math.abs(m - 6.67408E-11) < 1E-5); assert(math.abs(s - 3.1E-14) < 1E-10)
      case _ => fail("should be Gaussian")
    }
  }
  it should "parse 3.1415927(01)" in {
    val parser = new FuzzyParser
    val r = parser.parseAll(parser.fuzzy, "3.1415927(01)")
    r should matchPattern { case parser.Success(_, _) => }
    r.get match {
      case Gaussian(m, s) => assert(math.abs(m - 3.1415927) < 1E-5); assert(math.abs(s - 0.000001) < 1E-10)
      case f@_ => fail(s"should be Gaussian but is $f")
    }
  }
  it should "parse 3.1415927" in {
    val parser = new FuzzyParser
    val r = parser.parseAll(parser.fuzzy, "3.1415927")
    r should matchPattern { case parser.Success(_, _) => }
    r.get match {
      case Exact(x) => assert(math.abs(x - 3.1415927) < 1E-5)
      case f@_ => fail(s"should be Exact but is $f")
    }
  }
}
