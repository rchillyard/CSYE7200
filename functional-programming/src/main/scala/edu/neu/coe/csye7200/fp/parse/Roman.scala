package edu.neu.coe.csye7200.fp.parse

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by scalaprof on 4/16/17.
  */
object Roman extends JavaTokenParsers {

  //  case class RomanNumber(s: String)
  //
  //  case class Group(s: String)
  //
  //  def roman: Parser[RomanNumber] = group ~ rep(group)
  //
  //  def group: Parser[Group] = (accreting | attenuating) ^^ { x: String => Group(x) }
  //
  //  def attenuating: Parser[String] = "M"
  //
  //  def accreting: Parser[Group] = igroup | vgroup | xgroup | "X" ~ lgroup | "L" ~ cgroup | "C" ~ dgroup | "D" ~ mgroup ^^ { s ~ g => Group(s+g.toString) }
  //
  //  def igroup: Parser[Group] = rep("I") ~ vgroup ^^ { case xs ~ g => Group(xs.mkString("", "", "") + g.toString)}
  //  def vgroup: Parser[Group] = rep("V") ~ xgroup ^^ { case xs ~ g => Group(xs.mkString("", "", "") + g.toString)}
  //  def xgroup: Parser[Group] = rep("X") ~ lgroup ^^ { case xs ~ g => Group(xs.mkString("", "", "") + g.toString)}
  //  def lgroup: Parser[Group] = rep("L") ~ cgroup ^^ { case xs ~ g => Group(xs.mkString("", "", "") + g.toString)}
  //  def cgroup: Parser[Group] = rep("C") ~ dgroup ^^ { case xs ~ g => Group(xs.mkString("", "", "") + g.toString)}
  //  def dgroup: Parser[Group] = rep("D") ~ mgroup ^^ { case xs ~ g => Group(xs.mkString("", "", "") + g.toString)}
  //  def mgroup: Parser[Group] = rep("M") ^^ { case xs => Group(xs.mkString("", "", ""))}
}
