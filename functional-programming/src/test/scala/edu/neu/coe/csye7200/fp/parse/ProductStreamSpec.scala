package edu.neu.coe.csye7200.fp.parse

import edu.neu.coe.csye7200.fp.util.Lift
import java.io.{File, FileInputStream}
import org.joda.time._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util._

/**
  * @author scalaprof
  */
class ProductStreamSpec extends AnyFlatSpec with Matchers {
  """"Hello", "World!"""" should "be (String) stream via CSV" in {
    val c = CSV[Tuple1[String]](LazyList("x", """"Hello"""", """"World!""""))
    c.header shouldBe List("x")
    val wts = c.tuples
    wts.head match {
      case Tuple1(s) => assert(s == "Hello")
    }
    wts.tail.head match {
      case Tuple1(s) => assert(s == "World!")
    }
  }
  it should "be (String) stream via TupleStream" in {
    val wts = TupleStream[Tuple1[String]](LazyList("x", """"Hello"""", """"World!"""")).tuples
    wts.head match {
      case Tuple1(s) => assert(s == "Hello")
    }
    wts.tail.head match {
      case Tuple1(s) => assert(s == "World!")
    }
  }
  it should "convert to list properly" in {
    val c = CSV[Tuple1[String]](LazyList("x", """"Hello"""", """"World!""""))
    val wts = c.asList
    wts.size should be(2)
  }
  it should "convert to map properly" in {
    val c = CSV[Tuple1[String]](LazyList("x", """"Hello"""", """"World!""""))
    val wtIm = c toMap { case Tuple1(s) => s.hashCode }
    wtIm.get("Hello".hashCode) should matchPattern { case Some(Tuple1("Hello")) => }
  }
  it should "have column x of type String" in {
    val c = CSV[Tuple1[String]](LazyList("x", """"Hello"""", """"World!""""))
    c column[String] "x" match {
      case Some(xs) =>
        xs.take(2).toList.size should be(2)
        xs.head shouldBe "Hello"
        xs(1) shouldBe "World!"
      case _ => fail("no column projected")
    }
  }
  """"3,5", "8,13"""" should "be (Int,Int) stream" in {
    val iIts = CSV[(Int, Int)](LazyList("x,y", "3,5", "8,13")).tuples
    iIts.head match {
      case (x, y) => assert(x == 3 && y == 5)
    }
    iIts.tail.head match {
      case (x, y) => assert(x == 8 && y == 13)
    }
  }
  it should "be (String,String) stream via TupleStream" in {
    val wWts = TupleStream[(String, String)](LazyList("x,y", "3,5", "8,13")).tuples
    wWts.head match {
      case (x, y) => assert(x == "3" && y == "5")
    }
    wWts.tail.head match {
      case (x, y) => assert(x == "8" && y == "13")
    }
  }
  it should "map into (Int,Int) via TupleStream" in {
    val wWts = TupleStream[(String, String)](LazyList("x,y", "3,5", "8,13"))
    val iIts = wWts map { case (x, y) => (x.toInt, y.toInt) }
    iIts.tuples.head match {
      case (x, y) => assert(x == 3 && y == 5)
      case _ => fail("no match")
    }
  }
  it should "have column y of type Int" in {
    CSV[(Int, Int)](LazyList("x,y", "3,5", "8,13")) column[Int] "y" match {
      case Some(ys) =>
        ys.take(2).toList.size should be(2)
        ys.head shouldBe 5
        ys(1) shouldBe 13
      case _ => fail("no column projected")
    }
  }
  it should "convert to map properly" in {
    val c = CSV[(Int, Int)](LazyList("x,y", "3,5", "8,13"))
    val iItIm = c toMap { case (x, _) => x }
    iItIm.get(8) should matchPattern { case Some((8, 13)) => }
  }
  it should "map into (Double,Double) properly" in {
    val c = CSV[(Int, Int)](LazyList("x,y", "3,5", "8,13"))
    val doubles = c map { case (x, y) => (x.toDouble, y.toDouble) }
    val dDts = doubles.tuples
    dDts.head match {
      case (x, y) => assert(x == 3.0 && y == 5.0)
    }
  }
  it should "convert into maps properly" in {
    val zWms = CSV[(Int, Int)](LazyList("x,y", "3,5", "8,13")).asMaps
    zWms.head should be(Map("x" -> 3, "y" -> 5))
    zWms(1) should be(Map("x" -> 8, "y" -> 13))
  }
  """"3,5.0", "8,13.5"""" should "be (Int,Double) stream" in {
    val dIts = CSV[(Int, Double)](LazyList("x,y", "3,5.0", "8,13.5")).tuples
    dIts.head match {
      case (x, y) => assert(x == 3 && y == 5.0)
    }
    dIts.tail.head match {
      case (x, y) => assert(x == 8 && y == 13.5)
    }
  }
  """dateParser""" should "work" in {
    val dp = CsvParser.dateParser
    dp("2016-03-15") should matchPattern { case Success(_) => }
  }
  """"milestone 1, 2016-03-08", "milestone 2, 2016-03-15"""" should "be (String,Datetime) stream" in {
    val dIts = CSV[(String, DateTime)](LazyList("event,date", "milestone 1,2016-03-08", "milestone 2,2016-03-15")).tuples
    dIts.head match {
      case (x, y) => assert(x == "milestone 1" && y == new DateTime("2016-03-08"))
    }
    dIts.tail.head match {
      case (x, y) => assert(x == "milestone 2" && y == new DateTime("2016-03-15"))
    }
  }
  "FunctionalProgramming/sample.csv" should "be (String,Int) stream" in {
    val iWts = CSV[(String, Int)](new FileInputStream(new File(getClass.getResource("sample.csv").getPath))).tuples
    iWts.head match {
      case (x, y) => assert(x == "Sunday" && y == 1)
    }
    iWts.tail.head match {
      case (x, y) => assert(x == "Monday" && y == 2)
    }
    iWts.size should be(8)
    (iWts take 8).toList(7) should be("TGIF, Bruh", 8)
  }
  it should "be (String,Int) stream using File" in {
    val iWts = CSV[(String, Int)](new File(getClass.getResource("sample.csv").getPath)).tuples
    iWts.head match {
      case (x, y) => assert(x == "Sunday" && y == 1)
    }
    iWts.tail.head match {
      case (x, y) => assert(x == "Monday" && y == 2)
    }
    iWts.size should be(8)
    (iWts take 8).toList(7) should be("TGIF, Bruh", 8)
  }
  it should "be (String,Int) stream using URI" in {
    val iWts = CSV[(String, Int)](getClass.getResource("sample.csv").toURI).tuples
    iWts.head match {
      case (x, y) => assert(x == "Sunday" && y == 1)
    }
    iWts.tail.head match {
      case (x, y) => assert(x == "Monday" && y == 2)
    }
    iWts.size should be(8)
    (iWts take 8).toList(7) should be("TGIF, Bruh", 8)
  }
}

class CsvParserSpec extends AnyFlatSpec with Matchers {
  val defaultParser = CsvParser()
  "CsvParser()" should """parse "x" as Success(List("x"))""" in {
    defaultParser.parseRow(""""x"""") should matchPattern { case scala.util.Success(List("x")) => }
  }
  it should """parse "x,y" as Success(List("x,y"))""" in {
    defaultParser.parseRow(""""x,y"""") should matchPattern { case scala.util.Success(List("x,y")) => }
  }
  it should """parse "x,y" as Success(List("x","y")""" in {
    defaultParser.parseRow("x,y") should matchPattern { case scala.util.Success(List("x", "y")) => }
  }
  val pipeParser = CsvParser("|")
  """"CsvParser("|")"""" should """parse "|" as Success(List("|"))""" in {
    pipeParser.parseRow(""""|"""") should matchPattern { case scala.util.Success(List("|")) => }
  }
  it should """parse x,y as Success(List("x,y"))""" in {
    pipeParser.parseRow("x,y") should matchPattern { case scala.util.Success(List("x,y")) => }
  }
  it should """parse x,y as Success(List("x","y")""" in {
    pipeParser.parseRow("x|y") should matchPattern { case scala.util.Success(List("x", "y")) => }
  }
  val customParser = CsvParser("|", "'")
  """"CsvParser("|","'")"""" should """parse '|' as Success(List("|"))""" in {
    customParser.parseRow("'|'") should matchPattern { case scala.util.Success(List("|")) => }
  }
  it should """parse x,y as Success(List("x,y"))""" in {
    customParser.parseRow("x,y") should matchPattern { case scala.util.Success(List("x,y")) => }
  }
  it should """parse x,y as Success(List("x","y")""" in {
    customParser.parseRow("x|y") should matchPattern { case scala.util.Success(List("x", "y")) => }
  }
  "CsvParser.parseElem" should "parse 1 as 1" in (CsvParser.defaultParser("1") should matchPattern { case Success(1) => })
  it should "parse 1.0 as 1.0" in (CsvParser.defaultParser("1.0") should matchPattern { case Success(1.0) => })
  it should "parse true as true" in (CsvParser.defaultParser("true") should matchPattern { case Success(true) => })
  it should "parse false as false" in (CsvParser.defaultParser("false") should matchPattern { case Success(false) => })
  it should "parse yes as yes" in (CsvParser.defaultParser("yes") should matchPattern { case Success(true) => })
  it should "parse no as false" in (CsvParser.defaultParser("no") should matchPattern { case Success(false) => })
  it should "parse T as true" in (CsvParser.defaultParser("T") should matchPattern { case Success(true) => })
  it should """parse "1" as "1"""" in (CsvParser.defaultParser(""""1"""") should matchPattern { case Success("1") => })
  it should """parse 2016-03-08 as datetime""" in {
    val dt = CsvParser.defaultParser("2016-03-08")
    dt should matchPattern { case Success(_) => }
    //    dt.get shouldBe new DateTime("2016-03-08")
  }

  def putInQuotes(w: String): Any = s"""'$w'"""

  val customElemParser = CsvParser(parseElem = Lift(putInQuotes))
  "custom element parser" should "parse 1 as '1'" in (customElemParser.elementParser("1") should matchPattern { case Success("'1'") => })
  it should "parse 1.0 as '1.0'" in (customElemParser.elementParser("1.0") should matchPattern { case Success("'1.0'") => })
  it should "parse true as 'true'" in (customElemParser.elementParser("true") should matchPattern { case Success("'true'") => })
  it should """parse "1" as '"1"'""" in (customElemParser.elementParser(""""1"""") should matchPattern { case Success("""'"1"'""") => })

  "CsvParser.parseDate" should "work" in {
    val dt = CsvParser.parseDate(CsvParser.dateFormatStrings)("2016-03-08")
    dt should matchPattern { case Success(_) => }
    dt.get shouldBe new DateTime("2016-03-08T00:00:00.0")
  }
}
