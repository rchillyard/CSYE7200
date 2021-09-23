//package edu.neu.coe.csye7200.fp.parse
//
//import java.io.{BufferedWriter, File, FileWriter}
//
//import com.phasmidsoftware.parse._
//import com.phasmidsoftware.table.{Header, Table, TableWithHeader, TableWithoutHeader}
//import edu.neu.coe.csye7200._
//
//import scala.io.{BufferedSource, Source}
//import scala.util.Success
//import scala.util.matching.Regex
//
//case class Question(question_ID: String, answer: Option[String], auto_score: Option[Double], manual_score: Option[Double])
//
//case class Submission(username: String, last_name: String, first_name: String, questions: Seq[Question])
//
//object Submissions extends CellParsers {
//
//  def baseColumnNameMapper(w: String): String = w.replaceAll("(_)", " ")
//
//  implicit val submissionColumnHelper: ColumnHelper[Submission] = columnHelper(baseColumnNameMapper _)
//  implicit val questionColumnHelper: ColumnHelper[Question] = columnHelper(baseColumnNameMapper _, Some("$c $x"))
//  implicit val optionalAnswerParser: CellParser[Option[String]] = cellParserOption
//  implicit val questionParser: CellParser[Question] = cellParser4(Question)
//  implicit val questionsParser: CellParser[Seq[Question]] = cellParserRepetition[Question]()
//  implicit val submissionParser: CellParser[Submission] = cellParser4(Submission)
//
//  implicit object SubmissionConfig extends DefaultRowConfig {
//    override val string: Regex = """[^\t]*""".r
//    override val delimiter: Regex = """\t""".r
//  }
//
//  implicit val parser: StandardRowParser[Submission] = StandardRowParser[Submission]
//
//  implicit object SubmissionTableParser extends StringTableParser[Table[Submission]] {
//    type Row = Submission
//
//    def hasHeader: Boolean = true
//
//    def builder(rows: Seq[Submission], maybeHeader: Option[Header]): Table[Submission] = TableWithHeader(rows, maybeHeader.get)
//
//    override def forgiving: Boolean = true
//
//    def rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]
//  }
//
//  trait HTMLRenderableString extends HTMLRenderable[String] {
//    override def toHTML(t: String)(implicit rules: TagRules): HTML = HTML("td", t)
//  }
//  implicit object HTMLRenderableString extends HTMLRenderableString
//
//  trait HTMLRenderableOptionString extends HTMLRenderableOption[String] {
//    override def default: String = ""
//  }
//  implicit object HTMLRenderableOptionString extends HTMLRenderableOptionString
//
//  trait HTMLRenderableQuestion extends HTMLRenderableProduct[Question]
//  implicit object HTMLRenderableQuestion extends HTMLRenderableQuestion
//
//  trait HTMLRenderableSubmission extends HTMLRenderable[Submission] {
//    override def toHTML(t: Submission)(implicit rules: TagRules): HTML = {
////      HTML("tr").:+()
////      ws.foldLeft[Tag](HTML("tr"))(_ :+ parseElementIntoHTMLElement(_, header))
//      HTML("bad")
//    }
//  }
//  implicit object HTMLRenderableSubmission extends HTMLRenderableSubmission
//}
//
///**
//  * Main program which reads a CSV file (specified by the command line argument) and converts it into
//  * an HTML file which is placed into "output.html".
//  *
//  * The delimiter for parsing the CSV file is is currently fixed to be the tab character.
//  * The charset is fixed as UTF-16
//  * The title of the HTML page produced is fixed as "Report"
//  * The output filename is fixed as "output.html"
//  */
//object ParseCSVtoHTML extends App {
////  val parser = CsvParser(delimiter = '\t' + "")
////  val csvParser = ParseCSVwithHTML(parser)
////  val title = "Report"
//  val parameters: List[Arg[String]] = Args.parse(args).toList
//  parameters match {
//    case Arg(_, Some(filename)) :: argsColumn => parseEncodeAndWriteToFile(filename, argsColumn)
//    case _ => System.err.println("syntax: ParseCSVtoHTML filename [column]*")
//  }
//
//  private def parseEncodeAndWriteToFile(filename: String, columnArgs: Seq[Arg[String]]): Unit = {
//    val file = new File("output.html")
//    // TODO why are we ignoring result here?
//    //noinspection ScalaUnusedSymbol
//    val _ = new BufferedWriter(new FileWriter(file))
//
//    import Submissions._
//
//    val s: BufferedSource = Source.fromFile(new File(filename), "UTF-16")
//    val sty = Table.parse(s)
//    println(sty.get.size)
//    sty match {
//      case Success(rs) => rs foreach println
//      case _ =>
//    }
//  }
//}
