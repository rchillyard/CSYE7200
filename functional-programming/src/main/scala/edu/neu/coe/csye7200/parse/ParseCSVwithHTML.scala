package edu.neu.coe.csye7200.parse

import java.io.{BufferedWriter, File, FileWriter}

import scala.collection.mutable
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}

/**
  * This class defines a parser for taking a CSV file with interspersed HTML (for example the "Results" file
  * which is downloaded from Blackboard.
  *
  * The parseStreamIntoHTMLTable method turns the given Stream of Strings into an HTML table.
  * The header is kept intact and forms the header of the table.
  * Each row is processed by putting each element (column value) into a <td></td> pair of tags.
  *
  * @param csvParser a CSVParser
  */
case class ParseCSVwithHTML(csvParser: CsvParser) {

  val html = new HTML

  def parseElementIntoHTMLElement(w: String, header: Boolean = false): String = {
    val result = new HTML
    result.tag(if (header) "th" else "td")
    result.append(w)
    result.close()
    result.toString()
  }

  def parseRowIntoHTMLRow(w: String, header: Boolean = false): String = {
    val result = new HTML
    result.tag("tr")
    val wsy: Try[List[String]] = csvParser.parseRow(w)
    wsy match {
      case Success(ws) => for (w <- ws) result.append(parseElementIntoHTMLElement(w, header))
      case Failure(x) => System.err.println(s"Error parsing `$w`: ${x.getLocalizedMessage}")
    }
    result.close()
    result.toString
  }

  def preamble(w: String): String = {
    val result = new HTML
    result.tag("head")
    result.tag("title")
    result.append(w)
    result.close()
    result.toString
  }

  def parseStreamIntoHTMLTable(ws: Stream[String], title: String): String = {
    val result = new HTML
    result.tag("html")
    result.append(preamble(title))
    result.tag("body")
    result.tag("table")
    ws match {
      case header #:: body =>
        result.append(parseRowIntoHTMLRow(header, header = true))
        for (w <- body) result.append(parseRowIntoHTMLRow(w))
    }
    result.close()
    result.toString
  }
}

/**
  * Mutable class to form an HTML string
  */
class HTML() {
  val content = new StringBuilder("")
  val tagStack: mutable.Stack[String] = mutable.Stack[String]()

  def tag(w: String): StringBuilder = {
    tagStack.push(w)
    content.append(s"<$w>")
  }

  def unTag: StringBuilder = content.append(s"</${tagStack.pop()}>")

  def append(w: String): StringBuilder = content.append(w)

  def close(): Unit = while (tagStack.nonEmpty) {
    unTag
  }

  override def toString: String = content.toString + "\n"
}

/**
  * Main program which reads a CSV file (specified by the command line argument) and converts it into
  * an HTNL file which is placed into "output.html".
  *
  * The delimiter for parsing the CSV file is is currently fixed to be the tab character.
  * The charset is fixed as UTF-16
  * The title of the HTML page produced is fixed as "Report"
  * The output filename is fixed as "output.html"
  */
object ParseCSVwithHTML extends App {
  val parser = ParseCSVwithHTML(CsvParser(delimiter = '\t' + ""))
  val title = "Report"
  if (args.length > 0) {
    val filename = args.head
    val source: BufferedSource = Source.fromFile(filename, "UTF-16")
    val w = parser.parseStreamIntoHTMLTable(source.getLines.toStream, title)
    val file = new File("output.html")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(w)
    bw.close()
    println(s"Successfully written $file")
  }
  else
    System.err.println("syntax: ParseCSVwithHTML filename")

}
