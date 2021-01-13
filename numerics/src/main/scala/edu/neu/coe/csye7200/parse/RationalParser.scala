package edu.neu.coe.csye7200.parse

import edu.neu.coe.csye7200.numerics.Rational

import scala.util.Try
import scala.util.parsing.combinator.JavaTokenParsers

trait RationalNumber {

  /**
    * Evaluate this RationalNumber as a Try[Rational].
    *
    * @return a Try[Rational]
    */
  def value: Try[Rational]
}

class RationalParser extends JavaTokenParsers {

  def parse[R](p: Parser[R], w: String): Try[R] = parseAll(p, w) match {
    case Success(t, _) => scala.util.Success(t)
    case Failure(m, _) => scala.util.Failure(RationalParserException(m))
    case Error(m, _) => scala.util.Failure(RationalParserException(m))
  }

  case class WholeNumber(sign: Boolean, digits: String) extends RationalNumber {
    override def value: Try[Rational] = scala.util.Success(Rational(BigInt(digits)).applySign(sign))
  }

  object WholeNumber {
    val one: WholeNumber = WholeNumber(sign = false, "1")
  }

  case class RatioNumber(numerator: WholeNumber, denominator: WholeNumber) extends RationalNumber {
    override def value: Try[Rational] = for (n <- numerator.value; d <- denominator.value) yield n / d
  }

  case class RealNumber(sign: Boolean, integerPart: String, fractionalPart: String, exponent: Option[String]) extends RationalNumber {
    override def value: Try[Rational] = {
      val bigInt = BigInt(integerPart + fractionalPart)
      val exp = exponent.getOrElse("0").toInt
      Try(Rational(bigInt).applySign(sign).applyExponent(exp - fractionalPart.length))
    }
  }

  def rationalNumber: Parser[RationalNumber] = realNumber | ratioNumber

  def ratioNumber: Parser[RatioNumber] = simpleNumber ~ opt("/" ~> simpleNumber) ^^ { case n ~ maybeD => RatioNumber(n, maybeD.getOrElse(WholeNumber.one)) }

  def simpleNumber: Parser[WholeNumber] = opt("-") ~ wholeNumber ^^ { case so ~ n => WholeNumber(so.isDefined, n) }

  def realNumber: Parser[RealNumber] = opt("-") ~ wholeNumber ~ ("." ~> wholeNumber) ~ opt(E ~> wholeNumber) ^^ { case so ~ integerPart ~ fractionalPart ~ expo => RealNumber(so.isDefined, integerPart, fractionalPart, expo) }

  private val E = "[eE]".r
}

object RationalParser {
  val parser = new RationalParser

  def parse(s: String): Try[Rational] = parser.parse(parser.rationalNumber, s).flatMap(_.value)
}

case class RationalParserException(m: String) extends Exception(m)
