package edu.neu.coe.csye7200.fp.parse

import edu.neu.coe.csye7200.fp.{Arg, Args, HTML, Tag}
import java.io.{BufferedWriter, File, FileWriter}
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

  val liftColonPlus: (Try[Tag], Try[Tag]) => Try[Tag] = ParseCSVwithHTML.lift2Try(_ :+ _)

  def parseElementIntoHTMLElement(w: String, header: Boolean = false): Tag =
  // Replace the non-breaking space character with proper HTML
    HTML(if (header) "th" else "td", w.replaceAll("""\u00A0""", "&#160"))

  def parseRowIntoHTMLRow(w: String, header: Boolean = false): Try[Tag] = csvParser.parseRow(w) match {
    case Success(ws) => Success(ws.foldLeft[Tag](HTML("tr"))(_ :+ parseElementIntoHTMLElement(_, header)))
    case Failure(x) => Failure(x)
  }

  def parseRowProjectionIntoHTMLRow(ws: Seq[String], header: Boolean = false): Tag = ws.foldLeft[Tag](HTML("tr"))(_ :+ parseElementIntoHTMLElement(_, header))

  def preamble(title: String): Tag = HTML("head") :+ HTML("title", title)

  def parseStreamIntoHTMLTable(ws: LazyList[String], title: String): Try[Tag] = {
    val table: Try[Tag] = ws match {
      case header #:: body =>
        val ty = liftColonPlus(Try(HTML("table", Map("border" -> "1"))), parseRowIntoHTMLRow(header, header = true))
        body.foldLeft(ty)((x, y) => liftColonPlus(x, parseRowIntoHTMLRow(y)))
    }
    liftColonPlus(Try(HTML("html") :+ preamble(title)), liftColonPlus(Try(HTML("body")), table))
  }

  def parseStreamProjectionIntoHTMLTable(columns: Seq[String], wss: LazyList[Seq[String]], title: String): Try[Tag] = Try {
    val table = HTML("table", Map("border" -> "1")) :+ parseRowProjectionIntoHTMLRow(columns, header = true)
    val body = HTML("body") :+ wss.foldLeft(table)((tag, ws) => tag :+ parseRowProjectionIntoHTMLRow(ws))
    HTML("html") :+ preamble(title) :+ body
  }
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
  def map2[T1, T2, R](t1y: Try[T1], t2y: Try[T2])(f: (T1, T2) => R): Try[R] = for {t1 <- t1y; t2 <- t2y} yield f(t1, t2)

  def lift2Try[T1, T2, R](f: (T1, T2) => R): (Try[T1], Try[T2]) => Try[R] = map2(_, _)(f)

  val parser = CsvParser(delimiter = s"""\t""")
  val csvParser = ParseCSVwithHTML(parser)
  val title = "Report"
  val parameters: List[Arg[String]] = Args.parse(args).toList
  parameters match {
    case Arg(_, Some(filename)) :: argsColumn => parseEncodeAndWriteToFile(filename, argsColumn)
    case _ => System.err.println("syntax: ParseCSVwithHTML filename [column]*")
  }

  private def parseEncodeAndWriteToFile(filename: String, columnArgs: Seq[Arg[String]]): Unit = {
    val file = new File("output.html")
    val bw = new BufferedWriter(new FileWriter(file))

    val ty = parseEncodeAndWriteString(filename, columnArgs)
    ty.recover { case x: Throwable => System.err.println(s"No output written because: $x") }.foreach(t => bw.write(t.toString))
    bw.close()
    if (ty.isSuccess) println(s"Successfully written $file")
  }

  private def parseEncodeAndWriteString(filename: String, columnArgs: Seq[Arg[String]]) = {
    val stream = Source.fromFile(filename, "UTF-16").getLines().to(LazyList)
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
