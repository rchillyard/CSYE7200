package edu.neu.coe.csye7200.csv

import edu.neu.coe.csye7200.csv.tableParser.TableDatasetParser
import org.apache.spark.sql.{Dataset, SparkSession}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Try

class MovieDatabaseAnalyzerTest extends AnyFlatSpec with Matchers {

  implicit val spark: SparkSession = SparkSession
    .builder()
    .appName("MovieDatabaseAnalyzer")
    .master("local[*]")
    .getOrCreate()

  spark.sparkContext.setLogLevel("ERROR") // We want to ignore all of the INFO and WARN messages.

  behavior of "parseResource"
  it should "get movie_metadata.csv" in {
    val movieTableParser: TableDatasetParser[Movie] = new TableDatasetParser[Movie] {}

    import MovieParser._
    import spark.implicits._
    val mdy: Try[Dataset[Movie]] = movieTableParser.parseResource("/movie_metadata.csv")
    mdy.isSuccess shouldBe true
    mdy foreach {
      d =>
        d.count() shouldBe 1567
        d.show(10)
    }
  }

}