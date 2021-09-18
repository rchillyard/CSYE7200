package edu.neu.coe.csye7200.fp.parse

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by scalaprof on 4/16/17.
  */
class PostCode extends JavaTokenParsers {

  override def skipWhitespace = false

  case class PostCode(outward: OutwardCode, inward: InwardCode) {
    override def toString: String = s"$outward $inward"
  }

  case class OutwardCode(area: String, district: District) {
    require(area.length == 1 || area.length == 2)

    override def toString: String = s"$area$district"
  }

  case class District(x: Digit, yo: Option[Digit], zo: Option[Char]) {
    override def toString: String = x.toString + yo.getOrElse("") + zo.getOrElse("")
  }

  object District {
    def create(x: Digit, yo: Option[Digit], zo: Option[String]): District = apply(x, yo, zo map (_.head))
  }

  case class InwardCode(sector: Digit, unit: String) {
    require(unit.length == 2)

    override def toString: String = s"$sector$unit"
  }

  case class Digit(x: Int) {
    val range = Range(0, 9)
    require(range.contains(x))

    override def toString: String = x.toString
  }

  /**
    * Definition of UK PostCode Grammar. See https://en.wikipedia.org/wiki/Postcodes_in_the_United_Kingdom
    *
    * postCode ::= outwardCode " " inwardCode
    * outwardCode ::= area district
    * inwardCode ::= digit unit
    * district = digit digit? letter?
    * area ::= letter letter?
    * digit ::= [0-9]
    * letter ::= [A-Z]
    */
  def postCode: Parser[PostCode] = outwardCode ~ (" " ~> inwardCode) ^^ { case o ~ i => PostCode(o, i) }

  def outwardCode: Parser[OutwardCode] = area ~ district ^^ { case a ~ d => OutwardCode(a, d) }

  def inwardCode: Parser[InwardCode] = digit ~ unit ^^ { case d ~ u => InwardCode(d, u) }

  def district: Parser[District] = digit ~ digit.? ~ letter.? ^^ { case d1 ~ d2 ~ l => District.create(d1, d2, l) }

  def area: Parser[String] = """[A-Z]{1,2}""".r

  def unit: Parser[String] = """[A-Z]{2}""".r

  private[parse] def digit[Digit] = """\d""".r ^^ (x => Digit(x.toInt))

  def letter: Parser[String] = """[A-Z]""".r
}
