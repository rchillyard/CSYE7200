package edu.neu.coe.csye7200.fp.parse

import edu.neu.coe.csye7200.MonadOps
import edu.neu.coe.csye7200.fp.util.{Trial, Tuples}
import java.io.{File, InputStream}
import java.net.URI
import org.joda.time._
import org.joda.time.format._
import scala.annotation.tailrec
import scala.io.Source
import scala.util._
import scala.util.matching.Regex
import scala.util.parsing.combinator._

/**
  * ProductStream is a monadic trait which provides a Stream of Tuples (tuples) and a Header (header).
  * Thus it is well-suited for use as an ingest mechanism of CSV files.
  *
  * The polymorphic type X is a Tuple of some count.
  * Please note that type inference is not able to infer this type from reading the file (which doesn't happen until runtime).
  * Therefore, the caller must supply the type of X.
  * Please see ProductStreamSpec for exemplars.
  *
  * Please see inline method documentation for details of other methods.
  *
  * @author scalaprof
  * @tparam X the underlying type, a sub-class of Product
  */
trait ProductStream[X <: Product] {
  /**
    * @return a sequence of String corresponding to the column names (left to right)
    */
  def header: Seq[String]

  /**
    * @return a Stream of tuples
    */
  def tuples: LazyList[X]

  /**
    * @return a materialized (non-lazy) List version of the tuples.
    */
  lazy val asList: List[X] = tuples.toList

  /**
    * map method
    *
    * @param f function to be applied to each tuple
    * @return a ProductStream of transformed tuples
    */
  def map[Y <: Product](f: X => Y): ProductStream[Y] = // Assignment6 5
  ??? // TO BE IMPLEMENTED

  /**
    * flatMap method
    *
    * @param f function to be applied to each tuple
    * @return a ProductStream of transformed tuples
    */
  def flatMap[Y <: Product](f: X => Iterable[Y]): ProductStream[Y] = // Assignment6 5
  ??? // TO BE IMPLEMENTED

  /**
    * toMap method
    *
    * @param pk function to yield a primary key value from a tuple
    * @return a Map where each element is of form pk->tuple
    */
  def toMap[K](pk: X => K): Map[K, X] = (for {x <- asList} yield pk(x) -> x).toMap

  /**
    * @param i get the ith row as a tuple
    * @return Some(tuple) if i is valid, else None
    */
  def get(i: Int): Option[X] = if (i >= 0 && i < asList.size) Some(asList.apply(i)) else None

  /**
    * @return a Stream of Maps, each map corresponding to a row, such that the keys are the column names (from the header)
    *         and the values are the tuple values
    */
  def asMaps: LazyList[Map[String, Any]]
}

/**
  * Base class for implementers of ProductStream
  */
abstract class ProductStreamBase[X <: Product] extends ProductStream[X] {
  /**
    * Method asMaps converts this ProductStream into a Stream of Map[String,Any] objects, one per row.
    * The keys for the map are derived from the header and the values from the tuple elements.
    *
    * @return a Stream of Map[String,Any] objects
    */
  def asMaps: LazyList[Map[String, Any]] = // Assignment6 14
  ??? // TO BE IMPLEMENTED
}

/**
  * Base class for ProductStream which additionally derive their header and tuples from parsing a Stream of Strings (one per row).
  */
abstract class TupleStreamBase[X <: Product](parser: CsvParser, input: LazyList[String]) extends ProductStreamBase[X] {
  /**
    * @return the header for this object
    * @throws Exception which is wrapped in a Failure from wsy (below)
    */
  def header: Seq[String] = wsy.get

  /**
    * @param f the function which will be applied to a String to yield an Any (an element of a Tuple)
    * @param s the (row/line) String to be parsed
    * @return a Tuple
    * @throws Exception if any of the underlying code generated a Failure
    */
  def stringToTuple(f: String => Try[Any])(s: String): X = stringToTryTuple(f)(s).get

  protected lazy val wsy: Try[Seq[String]] = parser.parseRow(input.head)

  private def stringToTryTuple(f: String => Try[Any])(s: String): Try[X] =
    for {
      ws <- parser.parseRow(s)
      // Note that the following will result in a Failure[NoSuchElementException] if the filter results in false
      if ws.size == header.size
      // Note that the specification of [X] in the following is essential
      t <- TupleStream.seqToTuple[X](ws)(f)
    } yield t
}

/**
  * Case class which implements ProductStream where the header and tuples are specified directly
  */
case class ConcreteProductStream[X <: Product](header: Seq[String], tuples: LazyList[X]) extends ProductStreamBase[X]

/**
  * Case class which implements ProductStream where the header and tuples are specified indirectly, by providing
  * a parser and Stream[String] such that the element types of the resulting tuples will be inferred from their representative
  * Strings.
  *
  * @tparam X a Tuple which should correspond with the number of (and types inferred from) the values.
  */
case class CSV[X <: Product](parser: CsvParser, input: LazyList[String]) extends TupleStreamBase[X](parser, input) {
  /**
    * Method to define the tuples of this TupleStreamBase object.
    * Note that the [X] following stringToTuple looks optional, but it is not!
    *
    * @return a Stream of [X] objects
    */
  def tuples: LazyList[X] = input.tail map stringToTuple(parser.elementParser)

  /**
    * method to project ("slice") a ProductStream into a single column
    *
    * @param key the name of the column
    * @return an Option of Stream[Y] where Y is the type of the column
    */
  def column[Y](key: String): Option[LazyList[Y]] = column(header.indexOf(key))

  /**
    * method to project ("slice") a ProductStream into a single column
    *
    * @param i the index of the column (0 on the left, n-1 on the right)
    * @return an Option of Stream[Y] where Y is the type of the column
    */
  def column[Y](i: Int): Option[LazyList[Y]] =
    if (i >= 0) Some(tuples map CSV.project[X, Y](i))
    else None
}

/**
  * Case class which implements ProductStream where the header and tuples are specified indirectly, by providing
  * a parser and Stream[String] such that the element types of the resulting tuples will be Strings.
  *
  * @tparam X a Tuple which should correspond with the number of values (all types of the tuple should be String).
  */
case class TupleStream[X <: Product](parser: CsvParser, input: LazyList[String]) extends TupleStreamBase[X](parser, input) {
  def tuples: LazyList[X] = input.tail map stringToTuple { x => Success(x) }

  /**
    * method to project ("slice") a ProductStream into a single column
    *
    * @param key the name of the column
    * @return an Option of Stream[String]
    */
  def column(key: String): Option[LazyList[String]] = column(header.indexOf(key))

  /**
    * method to project ("slice") a ProductStream into a single column
    *
    * @param i the index of the column (0 on the left, n-1 on the right)
    * @return an Option of Stream[String]
    */
  def column(i: Int): Option[LazyList[String]] =
    if (i >= 0) Some(tuples map TupleStream.project[X](i))
    else None

  /**
    * method to project ("slice") a ProductStream into a sequence of named columns
    *
    * @param keys the names of the columns
    * @return an Stream[Seq[String]
    */
  def columnsByKey(keys: Seq[String]): LazyList[Seq[String]] = columns(for (key <- keys; i = header.indexOf(key); if i >= 0) yield i)

  /**
    * method to project ("slice") a ProductStream into a sequence of single columns
    *
    * @param is the indices of the columns (0 on the left, n-1 on the right)
    * @return an Stream[Seq[String]
    */
  def columns(is: Seq[Int]): LazyList[Seq[String]] = tuples map TupleStream.project[X](is)
}

object TupleStream {
  def apply[X <: Product](input: LazyList[String]): TupleStream[X] = apply(CsvParser(), input)

  def apply[X <: Product](parser: CsvParser, input: InputStream): TupleStream[X] = apply(parser, Source.fromInputStream(input).getLines().to(LazyList))

  def apply[X <: Product](input: InputStream): TupleStream[X] = apply(CsvParser(), input)

  def apply[X <: Product](parser: CsvParser, input: File): TupleStream[X] = apply(parser, Source.fromFile(input).getLines().to(LazyList))

  def apply[X <: Product](input: File): TupleStream[X] = apply(CsvParser(), input)

  def apply[X <: Product](parser: CsvParser, input: URI): TupleStream[X] = apply(parser, Source.fromFile(input).getLines().to(LazyList))

  def apply[X <: Product](input: URI): TupleStream[X] = apply(CsvParser(), input)

  def project[X <: Product](i: Int)(x: X): String = x.productElement(i).asInstanceOf[String]

  def project[X <: Product](is: Seq[Int])(x: X): Seq[String] = for (i <- is) yield x.productElement(i).asInstanceOf[String]

  def toTuple[X <: Product](ats: Seq[Try[Any]]): Try[X] = // Assignment6 8 Hint: use MonadOps.sequence; Tuples.toTuple; and asInstanceOf
  ??? // TO BE IMPLEMENTED

  def seqToTuple[X <: Product](ws: Seq[String])(f: String => Try[Any]): Try[X] = toTuple(ws map f)
}

object CSV {
  def apply[X <: Product](input: LazyList[String]): CSV[X] = apply(CsvParser(), input)

  def apply[X <: Product](parser: CsvParser, source: Source): CSV[X] = apply(parser, source.getLines().to(LazyList))

  def apply[X <: Product](source: Source): CSV[X] = apply(CsvParser(), source.getLines().to(LazyList))

  def apply[X <: Product](parser: CsvParser, input: InputStream): CSV[X] = apply(parser, Source.fromInputStream(input))

  def apply[X <: Product](input: InputStream): CSV[X] = apply(CsvParser(), input)

  def apply[X <: Product](parser: CsvParser, input: File): CSV[X] = apply(parser, Source.fromFile(input).getLines().to(LazyList))

  def apply[X <: Product](input: File): CSV[X] = apply(CsvParser(), input)

  def apply[X <: Product](parser: CsvParser, input: URI): CSV[X] = apply(parser, Source.fromFile(input).getLines().to(LazyList))

  def apply[X <: Product](input: URI): CSV[X] = apply(CsvParser(), input)

  def project[X <: Product, Y](i: Int)(x: X): Y = x.productElement(i).asInstanceOf[Y]
}

abstract class CsvParserBase(f: String => Try[Any]) extends JavaTokenParsers {
  /**
    * @return the trial function that will convert a String into Try[Any]
    *         This method is referenced only by CSV class (not by TupleStream, which does no element conversion).
    */
  lazy val elementParser: String => Try[Any] = f
}

case class CsvParser(
                            delimiter: String = ",", // delimiter separating elements within rows
                            quoteChar: String = """"""", // quotation char to allow strings to include literal delimiter characters, decimal points, etc.
                            parseElem: String => Try[Any] = CsvParser.defaultParser
                    ) extends CsvParserBase(parseElem) {
  override def skipWhitespace = false

  /**
    * The chief method of this CsvParser (probably the other parser methods should be private).
    *
    * @param s a String to be parsed as a row of a CSV file.
    * @return a Try of a List of String
    */
  def parseRow(s: String): Try[List[String]] = this.parseAll(this.row, s) match {
    case this.Success(r, _) =>
      scala.util.Success(r)
    case f@(this.Failure(_, _) | this.Error(_, _)) => scala.util.Failure(new Exception(s"cannot parse $s: $f"))
  }

  /**
    * Internal parser method to parse a row.
    * It succeeds on a sequence of terms, separated by the delimiter.
    *
    * @return a Parser of List of String
    */
  lazy val row: Parser[List[String]] = // Assignment6 3: row ::= term { delimiter term }
  ??? // TO BE IMPLEMENTED

  /**
    * Internal parser method to parse a term.
    * It succeeds on EITHER a string contained by a pair of quote characters OR a string which does not contain
    * any delimiter.
    *
    * @return a Parser of String
    */
  lazy val term: Parser[String] = // Assignment6 7: term ::= quoteChar text quoteChar | text
  ??? // TO BE IMPLEMENTED

  /**
    * Internal parser method to parse a string within quotes.
    * It succeeds on a quotedString contained by a pair of quote characters.
    *
    * @return a Parser of String
    */
  lazy val stringInQuotes: Parser[String] = quoteChar ~> quotedString <~ quoteChar

  /**
    * Internal parser method to parse a quoted string.
    * It succeeds EITHER:
    * a sequence of characters which may contain pairs of quotes (which are replaced by one a quote)
    * OR: a String which does not include any quote characters at all.
    *
    * @return a Parser of String
    */
  lazy val quotedString: Parser[String] = containingEscapedQuotes | nonQuotes

  /**
    * This parser succeeds on a sequence of strings, each made up of non-quote characters, and delimited
    * by double-quote-characters, i.e. two quote characters one after the other.
    *
    * @return a parser of String where the input has had the quote character pairs replaced by a single quote character.
    */
  lazy val containingEscapedQuotes: Parser[String] = repsep(nonQuotes, quoteChar + quoteChar) ^^ { xs: Seq[String] => xs.reduceLeft(_ + quoteChar + _) }

  /**
    * This parser succeeds on a sequence of characters which do not include the quote character
    *
    * @return a Parser of String
    */
  lazy val nonQuotes: Parser[String] =
    s"""[^$quoteChar]*""".r

  /**
    * This parser succeeds on a sequence of characters which do not include the delimiter character
    *
    * @return a Parser of String
    */
  lazy val nonDelimiters: Parser[String] =
    s"""[^$delimiter]+""".r
}

object CsvParser {
  val dateFormatStrings: Seq[String] = Seq("yyyy-MM-dd", "yyyy-MM-dd-hh:mm:ss.s")
  // etc.
  private[parse] val dateParser = Trial[String, Any]((parseDate _) (dateFormatStrings))
  private[parse] val defaultParser = Trial.none[String, Any] :| { case s@(date0(_) | date4(_) | date1(_)) => dateParser(s) } :^ { case quoted(w) => w } :^ { case whole(s) => s.toInt } :^ { case truth(_) => true } :^ { case untruth(_) => false } :^ {
    case s@(floating(_) | floating(_, _) | floating(_, _, _)) => s.toDouble
  } :^ identity
  private[parse] val date0 = """(\d\d\d\d-\d\d-\d\d)""".r
  // ISO 8601
  private[parse] val date1 =
    """(?m)^(0[1-9]|1\d|2[0-8]|29(?=-\d\d-(?!1[01345789]00|2[1235679]00)\d\d(?:[02468][048]|[13579][26]))|30(?!-02)|31(?=-0[13578]|-1[02]))-(0[1-9]|1[0-2])-([12]\d{3}) ([01]\d|2[0-3]):([0-5]\d):([0-5]\d)$""".r
  private[parse] val date2 = """(?m)^\d{4}-(((0[13578]|1[02])-(0[1-9]|[12]\d|3[0-1]))|(02-(0[1-9]|[12]\d))|((0[469]|11)-(0[1-9]|[12]\d|30)))$""".r
  private[parse] val date3 = """(^(((\d\d)(([02468][048])|([13579][26]))-02-29)|(((\d\d)(\d\d)))-((((0\d)|(1[0-2]))-((0\d)|(1\d)|(2[0-8])))|((((0[13578])|(1[02]))-31)|(((0[1,3-9])|(1[0-2]))-(29|30)))))\s(([01]\d|2[0-3]):([0-5]\d):([0-5]\d))$)""".r
  private[parse] val date4 = """(?mi)^([\+-]?\d{4}(?!\d{2}\b))((-?)((0[1-9]|1[0-2])(\3([12]\d|0[1-9]|3[01]))?|W([0-4]\d|5[0-2])(-?[1-7])?|(00[1-9]|0[1-9]\d|[12]\d{2}|3([0-5]\d|6[1-6])))([T\s]((([01]\d|2[0-3])((:?)[0-5]\d)?|24\:?00)([\.,]\d+(?!:))?)?(\17[0-5]\d([\.,]\d+)?)?([zZ]|([\+-])([01]\d|2[0-3]):?([0-5]\d)?)?)?)?$""".r

  def parseDate(dfs: Seq[String])(s: String): Try[DateTime] = {
    @tailrec def loop(formats: Seq[DateTimeFormatter], result: Try[DateTime]): Try[DateTime] = formats match {
      case Nil => result
      case h :: t => loop(t, result orElse Try(h.parseDateTime(s)))
    }

    loop(dfs map {
      DateTimeFormat.forPattern
    }, Failure(new Exception(s""""$s" cannot be parsed as date""")))
  }

  private val quoted = """"([^"]*)"""".r
  private val whole = """(\d+)""".r
  private val floating = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r
  private val truth = """(?i)^([ty]|true|yes)$""".r
  val untruth: Regex = """(?i)^([fn]|false|no)$""".r
}
