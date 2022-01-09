package edu.neu.coe.csye7200.fp.parse

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by scalaprof on 12/1/16.
  */
abstract class BadParser extends JavaTokenParsers {
  def name: Parser[(String, Option[String], String)] = ident ~ opt(ident) ~ ident ^^ { case f ~ mo ~ l => (f, mo, l) }
}

object BadParser extends BadParser

object BadParserMain extends App {
  val p = BadParser
  val r = p.parseAll(p.name, "Martin Scala Odersky")
  val first = r match {
    case p.Success((f, _, _), _) => f
    case _ => throw new RuntimeException("problem")
  }
  println(first)
}
