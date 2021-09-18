package edu.neu.coe.csye7200.concordance

import scala.io.Source
import scala.util.parsing.combinator._
import scala.util.parsing.input.Positional

/**
  * @author scalaprof
  *         (c) Phasmid Software, 2015
  */
class ConcordanceParser extends RegexParsers {
  private val rWord = """[\w’]+[,;\.\-\?\!\—]?""".r
  lazy val word: Parser[PositionalString] = positioned(regex(rWord) ^^ {w => PositionalString(w)})
  lazy val sentence: Parser[Seq[PositionalString]] = rep(word)
}

case class PositionalString(s: String) extends Positional

object ConcordanceParser {

  def main(args: Array[String]): Unit = {
    val docs = for (f <- args) yield Source.fromFile(f).mkString
    val concordance = for (i <- docs.indices) yield (args(i), parseDoc(docs(i)))
    println(concordance)
    // an alternative way of looking at the data (gives doc, page, line and char numbers with each string)
    val q = for {(d, xxxx) <- concordance; (p, xxx) <- xxxx; (l, xx) <- xxx; (_, c, x) <- xx} yield (d, p, l, c, x)
    println(q)
    // yet another way to look at the data
    val concordanceMap = concordance.toMap
    println(concordanceMap)
  }

  private def parseDoc(content: String) = {
    val pages = for (p <- content.split("/p")) yield p
    for (i <- pages.indices) yield (i + 1, parsePage(pages(i)))
  }

  private def parsePage(content: String) = {
    val lines = for (l <- content.split("\n")) yield l
    for (i <- lines.indices) yield (i + 1, parseLine(lines(i)))
  }

  def parseLine(line: String): Seq[(Int, Int, String)] = {
    def tidy(s: String) = s.replaceAll("""[,;.\-?!—]""", "")

    val p = new ConcordanceParser
    val r = p.parseAll(p.sentence, line) match {
      case p.Success(ws, _) => ws
      case p.Failure(e, _) => println(e); List()
      case _ => println("PositionalParser: logic error"); List()
    }
    r map { case p@PositionalString(s) => (p.pos.line, p.pos.column, tidy(s).toLowerCase) }
  }
}
