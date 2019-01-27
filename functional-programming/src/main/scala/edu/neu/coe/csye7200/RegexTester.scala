package edu.neu.coe.csye7200

import scala.util.matching.Regex

/**
  * Created by scalaprof on 12/24/16.
  */
case class RegexTester(pattern: Regex) {

  def parse(s: String): String = s match {
    case pattern(x) => x
    case _ => throw new Exception(s"no match for $s")
  }

}

object RegexTester extends App {
  val r1 = RegexTester(".*".r)
  println(r1.parse("hello"))
}
