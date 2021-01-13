package edu.neu.coe.csye7200.parse

import edu.neu.coe.csye7200.numerics.{Factor, Number, Scalar}

import scala.util.Try

class NumberParser extends RationalParser {

  def parseNumber(w: String): Try[Number] = parse(number, w)

  def number: Parser[Number] = rationalNumber ~ opt(factor) ^^ {
    case r ~ fo => (for (r <- r.value.toOption; f <- fo.orElse(Some(Scalar))) yield Number(r, f)).getOrElse(Number())
  }

  import Factor._

  def factor: Parser[Factor] = (sPi | sPiAlt0 | sPiAlt1 | sPiAlt2 | "e" | failure("factor")) ^^ { w => Factor(w) }

}
