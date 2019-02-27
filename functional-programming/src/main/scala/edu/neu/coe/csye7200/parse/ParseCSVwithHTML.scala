package edu.neu.coe.csye7200.parse

import java.io.{BufferedWriter, File, FileWriter}

import edu.neu.coe.csye7200.{Arg, Args}

import scala.collection.mutable
import scala.io.Source
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
    // Replace the non-breaking space character with proper HTML
    result.append(w.replaceAll("""\u00A0""", "&#160"))
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

  def parseRowProjectionIntoHTMLRow(ws: Seq[String], header: Boolean = false): String = {
    val result = new HTML
    result.tag("tr")
    for (w <- ws) result.append(parseElementIntoHTMLElement(w, header))
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
    result.tag("table", Some("""border="1""""))
    ws match {
      case header #:: body =>
        result.append(parseRowIntoHTMLRow(header, header = true))
        for (w <- body) result.append(parseRowIntoHTMLRow(w))
    }
    result.close()
    result.toString
  }

  def parseStreamProjectionIntoHTMLTable(columns: Seq[String], wss: Stream[Seq[String]], title: String): String = {
    val result = new HTML
    result.tag("html")
    result.append(preamble(title))
    result.tag("body")
    result.tag("table", Some("""border="1""""))
    result.append(parseRowProjectionIntoHTMLRow(columns, header = true))
    for (ws <- wss) result.append(parseRowProjectionIntoHTMLRow(ws))
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

  def tag(w: String, ao: Option[String] = None): StringBuilder = {
    tagStack.push(w)
    val attribute = ao match {
      case Some(a) => " "+a
      case _ => ""
    }
    content.append(s"<$w$attribute>")
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
  * an HTML file which is placed into "output.html".
  *
  * The delimiter for parsing the CSV file is is currently fixed to be the tab character.
  * The charset is fixed as UTF-16
  * The title of the HTML page produced is fixed as "Report"
  * The output filename is fixed as "output.html"
  */
object ParseCSVwithHTML extends App {
  val parser = CsvParser(delimiter = '\t' + "")
  val csvParser = ParseCSVwithHTML(parser)
  val title = "Report"
  val parameters: List[Arg[String]] = Args.parse(args).toList
  parameters match {
    case Arg(_, Some(filename)) :: argsColumn => parseEncodeAndWriteToFile(filename, argsColumn)
    case _ => System.err.println("syntax: ParseCSVwithHTML filename [column]*")
  }

  private def parseEncodeAndWriteToFile(filename: String, columnArgs: List[Arg[String]]): Unit = {
    val file = new File("output.html")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(parseEncodeAndWriteString(filename, columnArgs))
    bw.close()
    println(s"Successfully written $file")
  }

  private def parseEncodeAndWriteString(filename: String, columnArgs: Seq[Arg[String]]) = {
    val stream = Source.fromFile(filename, "UTF-16").getLines.toStream
    val columnNames = columnArgs.flatMap {
      case Arg(_, Some(columnName)) => Some(columnName)
      case _ => None
    }
    if (columnNames.isEmpty) println(s"Generate HTML")
    else println(s"""Generate HTML for only columns ${columnNames.mkString(", ")}""")
    if (columnNames.isEmpty) csvParser.parseStreamIntoHTMLTable(stream, title)
    else csvParser.parseStreamProjectionIntoHTMLTable(columnNames, TupleStream(parser, stream).columnsByKey(columnNames), title)
  }
}
