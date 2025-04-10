package edu.neu.coe.csye7200.csv

import com.phasmidsoftware.parse.{RowParser, StringTableParser}
import com.phasmidsoftware.table.{Header, Table}
import edu.neu.coe.csye7200.csv.tableParser.TableDatasetParser
import org.apache.spark.sql.{Dataset, SparkSession}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Try

class GenericCaseClassReaderTest extends AnyFlatSpec with Matchers {

  behavior of "TableParser"

  // FIXME: generic stuff not ready yet
  ignore should "parse generic.csv" in {
    new StringTableParser[Table[Generic]] {

      type Row = this.type
      protected val maybeFixedHeader: Option[Header] = ???

      protected def builder(rows: Iterable[this.type], header: Header): Table[Generic] = ???

      protected val rowParser: RowParser[this.type, String] = ???
      val headerRowsToRead: Int = 1
    }
  }

  behavior of "TableDatasetParser"

  // FIXME: generic stuff not ready yet
  ignore should "parse generic.csv" in {
    implicit val spark: SparkSession = SparkSession
            .builder()
            .appName("GenericCaseClassReader")
            .master("local[*]")
            .getOrCreate()

    spark.sparkContext.setLogLevel("ERROR") // We want to ignore all of the INFO and WARN messages.

    val genericTableParser: TableDatasetParser[Generic] = new TableDatasetParser[Generic] {}

    import GenericParser._
    import spark.implicits._
    val mdy: Try[Dataset[Generic]] = genericTableParser.parseResource("/generic.csv")
    mdy.isSuccess shouldBe true
    mdy foreach {
      d =>
        d.count() shouldBe 2
        d.show(2)
    }
  }

}