package edu.neu.coe.csye7200.fp.parse

import scala.io.Source
import scala.util.parsing.combinator._

/**
  * @author scalaprof
  */
object ConcordanceParser extends RegexParsers {
  private val rWord = """[\w’]+[,;.\-\?!!]?""".r

  def word: Parser[(Int, String)] = new PositionalParser(regex(rWord))

  def sentence: Parser[Seq[(Int, String)]] = rep(word)

  class PositionalParser(p: Parser[String]) extends Parser[(Int, String)] {
    def apply(in: Input): ParseResult[(Int, String)] =
      p.apply(in) match {
        case Success(w, pos) => Success((pos.offset - w.length + 1, w), pos)
        case f@Failure(_, _) => f
        case _ => Failure("PositionalParser: logic error", in)
      }
  }

}

object ConcordanceParserMain {

  def main(args: Array[String]): Unit = {
    val docs = for (f <- args) yield noleak(Source.fromFile(f))(_.mkString)
    val concordance = for (i <- docs.indices) yield (args(i), parseDoc(docs(i)))
    println(concordance)
    // an alternative way of looking at the data (gives doc, page, line and char numbers with each string)
    val q = for {(d, xxxx) <- concordance; (p, xxx) <- xxxx; (l, xx) <- xxx; (c, x) <- xx} yield (d, p, l, c, x)
    println(q)
    // yet another way to look at the data
    val concordanceMap = concordance.toMap
    println(concordanceMap)
  }

  def parseDoc(content: String): IndexedSeq[(Int, IndexedSeq[(Int, Seq[(Int, String)])])] = {
    val pages = for (p <- content.split("/p")) yield p
    for (i <- pages.indices) yield (i + 1, parsePage(pages(i)))
  }

  def parsePage(content: String): IndexedSeq[(Int, Seq[(Int, String)])] = {
    val lines = for (l <- content.split("\n")) yield l
    for (i <- lines.indices) yield (i + 1, parseLine(lines(i)))
  }

  def parseLine(line: String): Seq[(Int, String)] = {
    def tidy(s: String) = s.replaceAll("""[.,;-?!]""", "")

    val p = ConcordanceParser
    val r = p.parseAll(p.sentence, line) match {
      case p.Success(ws, _) => ws
      case p.Failure(e, _) => println(e); Nil
      case _ => println("PositionalParser: logic error"); Nil
    }
    r map { case (i, s) => (i, tidy(s).toLowerCase) }
  }

  /**
    * TODO Replace with Using when we upgrade to 2.13
    */
  private def noleak[R <: AutoCloseable, T](resource: => R)(f: R => T): T = {
    lazy val r = resource
    try f(r)
    finally r.close()
  }
}
