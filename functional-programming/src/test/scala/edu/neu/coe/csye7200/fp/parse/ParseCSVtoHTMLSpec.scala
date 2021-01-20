//package edu.neu.coe.csye7200.fp.parse
//
//
//import java.io.{BufferedWriter, File, FileWriter}
//
//import com.phasmidsoftware.parse._
//import com.phasmidsoftware.table.{Header, Indexed, Table, TableWithHeader, TableWithoutHeader}
//import com.phasmidsoftware.render._
//import edu.neu.coe.csye7200.fp.parse.ParseCSVwithHTML.parseEncodeAndWriteString
//import edu.neu.coe.csye7200.{Attribute, HTML, Tag, TagRules}
//import org.scalatest.flatspec.AnyFlatSpec
//import org.scalatest.matchers.should.Matchers
//
//import scala.io.Codec
//import scala.util._
//import scala.util.matching.Regex
//
//class ParseCSVtoHTMLSpec extends AnyFlatSpec with Matchers {
//
//  behavior of "Table"
//
//  object Submissions1 extends CellParsers {
//
//    def baseColumnNameMapper(w: String): String = w.replaceAll("(_)", " ")
//
//    implicit val submissionColumnHelper: ColumnHelper[Submission] = columnHelper(baseColumnNameMapper _)
//    implicit val questionColumnHelper: ColumnHelper[Question] = columnHelper(baseColumnNameMapper _, Some("$c $x"))
//    implicit val optionalAnswerParser: CellParser[Option[String]] = cellParserOption
//    implicit val questionParser: CellParser[Question] = cellParser4(Question)
//    implicit val questionsParser: CellParser[Seq[Question]] = cellParserRepetition[Question]()
//    implicit val submissionParser: CellParser[Submission] = cellParser4(Submission)
//
//    implicit object SubmissionConfig extends DefaultRowConfig {
//      override val string: Regex = """[^\|]*""".r
//      override val delimiter: Regex = """\|""".r
//    }
//
//    implicit val parser: StandardRowParser[Submission] = StandardRowParser[Submission]
//
//    implicit object SubmissionTableParser extends StringTableParser[Table[Submission]] {
//      type Row = Submission
//
//      def hasHeader: Boolean = true
//
//			override def builder(rows: Seq[Row], maybeHeader: Option[Header]): Table[Submission] = TableWithHeader(rows, maybeHeader.get)
//
//      override def forgiving: Boolean = true
//
//      def rowParser: RowParser[Row,String] = implicitly[RowParser[Row,String]]
//		}
//
//  }
//
//  it should "parse Submission1 with first two rows shortened" in {
//
//    val rows: Seq[String] = Seq(
//     """"Username"|"Last Name"|"First Name"|"Question ID 1"|"Question 1"|"Answer 1"|"Possible Points 1"|"Auto Score 1"|"Manual Score 1"""",
//     """"001234567s"|"Mr."|"Nobody"|"Question ID 1"|"The following are all good reasons to learn Scala -- except for one."|"Scala is the only functional language available on the Java Virtual Machine"|"4"|"4"|"""""
//    )
//
//    import Submissions1._
//    val qty: Try[Table[Submission]] = Table.parse(rows)
//    qty should matchPattern { case Success(_) => }
//    qty.get.size shouldBe 1
//  }
//
//	object Submissions3 extends CellParsers {
//
//		def baseColumnNameMapper(w: String): String = w.replaceAll("(_)", " ")
//
//		implicit val submissionColumnHelper: ColumnHelper[Submission] = columnHelper(baseColumnNameMapper _)
//		implicit val questionColumnHelper: ColumnHelper[Question] = columnHelper(baseColumnNameMapper _, Some("$c $x"))
//		implicit val optionalAnswerParser: CellParser[Option[String]] = cellParserOption
//		implicit val questionParser: CellParser[Question] = cellParser4(Question)
//		implicit val questionsParser: CellParser[Seq[Question]] = cellParserRepetition[Question]()
//		implicit val submissionParser: CellParser[Submission] = cellParser4(Submission)
//
//		implicit object SubmissionConfig extends DefaultRowConfig {
//			override val string: Regex = """[^\t]*""".r
//			override val delimiter: Regex = """\t""".r
//		}
//
//		implicit val parser: StandardRowParser[Submission] = StandardRowParser[Submission]
//
//		implicit object SubmissionTableParser extends StringTableParser[Table[Submission]] {
//			type Row = Submission
//
//			def hasHeader: Boolean = true
//
//			def builder(rows: Seq[Row], maybeHeader: Option[Header]): Table[Submission] = TableWithHeader(rows, maybeHeader.get)
//
//			override def forgiving: Boolean = true
//
//			def rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]
//		}
//
//	}
//
//
//	it should "parse sample.csv with Submission1" in {
//		import Submissions3._
//		implicit val codec: Codec = Codec("UTF-16")
//		val qty: Try[Table[Submission]] = Table.parseResource("/sample.csv")
//		qty should matchPattern { case Success(_) => }
//		qty.get.size shouldBe 1
//		qty.get.foreach(println)
//	}
//
//	object SubmissionsOutput extends Renderers {
//		implicit object NoTagRules extends TagRules
//		implicit object HTMLWriter extends TreeWriter[HTML] {
//			def evaluate(node: Node): HTML = {
//				val attrs: Seq[Attribute] = Attribute.mapToAttributes(node.attributes)
//				HTML(HTML.defaultName(node.style), attrs, node.content.getOrElse(""), node.children.map(evaluate))
//			}
//		}
//
//		implicit val stringCellRenderer: Renderer[String] = rendererExplicit("td", Map())(s => s)
//		implicit val doubleCellRenderer: Renderer[Double] = rendererExplicit("td", Map())(x => x.toString)
//		implicit val optionStringRenderer: Renderer[Option[String]] = optionRenderer("", Map())
//		implicit val optionDoubleRenderer: Renderer[Option[Double]] = optionRenderer("", Map())
//		implicit val questionRenderer: Renderer[Question] = renderer4("i")(Question.apply)
//		implicit val questionsRenderer: Renderer[Seq[Question]] = sequenceRenderer("")
//		implicit val submissionRenderer: Renderer[Submission] = renderer4("tr", Map("class"->"Submission"))(Submission)
//		implicit val indexedSubmissionRenderer: Renderer[Indexed[Submission]] = indexedRenderer("tr", "th")
//	}
//
//	it should "parse full file and output as HTML" in {
//		val filename = "/Users/rhillyard/Downloads/Mid-term makeup.download.xls-2.csv"
//
//		val file = new File("output.html")
//		val bw = new BufferedWriter(new FileWriter(file))
//
//
//		import Submissions3._
//		implicit val codec: Codec = Codec("UTF-16")
//		val qty: Try[Table[Submission]] = Table.parse(new File(filename))
//
//		qty foreach println
//
//		implicit object NoTagRules extends TagRules
//		implicit object HTMLWriter extends TreeWriter[HTML] {
//
//			def node(tag: String, content: Option[String], attributes: Map[String,String], children: Seq[HTML]): HTML =
//				HTML(HTML.defaultName(tag), Attribute.mapToAttributes(attributes), content.getOrElse(""), children)
//
//			override def evaluate(node: Node): HTML = HTML(HTML.defaultName(node.style), Attribute.mapToAttributes(node.attributes), node.content.getOrElse(""), node.children.map(evaluate))
//		}
//
//		def wrap(s: HTML): Tag = HTML("html"):+(HTML("body"):+s)
//
//		import SubmissionsOutput._
//
//		val hy: Try[HTML] = for (qt <- qty) yield qt.render("table", Map("border"->"1"))
//
//		hy should matchPattern { case Success(_) => }
//		val ty = hy map wrap
//
//		ty.recover { case x: Throwable => System.err.println(s"No output written because: $x") }.foreach(t => bw.write(t.toString))
//		bw.close()
//		if (ty.isSuccess) println(s"Successfully written $file")
//	}
//
//	object Submissions2 extends CellParsers {
//
//    def baseColumnNameMapper(w: String): String = w.replaceAll("(_)", " ")
//
//    implicit val submissionColumnHelper: ColumnHelper[Submission] = columnHelper(baseColumnNameMapper _)
//    implicit val questionColumnHelper: ColumnHelper[Question] = columnHelper(baseColumnNameMapper _, Some("$c $x"))
//    implicit val optionalAnswerParser: CellParser[Option[String]] = cellParserOption
//    implicit val questionParser: CellParser[Question] = cellParser4(Question)
//    implicit val questionsParser: CellParser[Seq[Question]] = cellParserRepetition[Question]()
//    implicit val submissionParser: CellParser[Submission] = cellParser4(Submission)
//    implicit val parser: StandardStringsParser[Submission] = StandardStringsParser[Submission]()
//
//    implicit object SubmissionTableParser extends StringsTableParser[Table[Submission]] {
//      type Row = Submission
//
//      def hasHeader: Boolean = true
//
//			def builder(rows: Seq[Row], maybeHeader: Option[Header]): Table[Submission] = TableWithHeader(rows, maybeHeader.get)
//
//      override def forgiving: Boolean = false
//
//      def rowParser: RowParser[Row, Seq[String]] = implicitly[RowParser[Row, Seq[String]]]
//    }
//
//  }
//
//  it should "parse Submission2" in {
//
//    val rows: Seq[Seq[String]] = Seq(
//      Seq("Username",	"Last Name",	"First Name", "Question ID 1",	"Question 1",	"Answer 1",	"Possible Points 1",	"Auto Score 1",	"Manual Score 1"),
//      Seq("001234567s",	"Mr.",	"Nobody", "Question ID 1",	"The following are all good reasons to learn Scala -- except for one.",	"Scala is the only functional language available on the Java Virtual Machine",	"4",	"4",	"")
//    )
//
//
//    import Submissions2._
//    val qty: Try[Table[Submission]] = Table.parseSequence(rows)
//    qty should matchPattern { case Success(_) => }
//    qty.get.size shouldBe 1
//  }
//
//  object QuestionsParser extends CellParsers {
//
//    def baseColumnNameMapper(w: String): String = w.replaceAll("(_)", " ")
//
//    implicit val submissionColumnHelper: ColumnHelper[Submission] = columnHelper(baseColumnNameMapper _,
//      "title" -> "Submission_title",
//      "imdb" -> "Submission_imdb_link")
//    implicit val questionColumnHelper: ColumnHelper[Question] = columnHelper(baseColumnNameMapper _, Some("$c_$x"))
//    implicit val optionalAnswerParser: CellParser[Option[String]] = cellParserOption
//    implicit val questionParser: CellParser[Question] = cellParser4(Question)
//    implicit val parser: StandardStringsParser[Question] = StandardStringsParser[Question]()
//
//    implicit object QuestionTableParser extends StringsTableParser[Table[Question]] {
//      type Row = Question
//
//      def hasHeader: Boolean = true
//
//			def builder(rows: Seq[Row], maybeHeader: Option[Header]): Table[Question] = TableWithHeader(rows, maybeHeader.get)
//
//      override def forgiving: Boolean = true
//
//      def rowParser: RowParser[Row,Seq[String]] = implicitly[RowParser[Row,Seq[String]]]
//    }
//  }
//
//  it should "parse Question" in {
//
//    val rows: Seq[Seq[String]] = Seq(Seq(
//      "Question ID",	"Question",	"Answer",	"Possible Points",	"Auto Score",	"Manual Score"),
//      Seq("Question ID",	"The following are all good reasons to learn Scala -- except for one.",	"Scala is the only functional language available on the Java Virtual Machine",	"4",	"4",	""))
//
//
//    import QuestionsParser._
//    val qty: Try[Table[Question]] = Table.parseSequence(rows)
//    qty should matchPattern { case Success(_) => }
//    qty.get.size shouldBe 1
//  }
//
//}
//
