package edu.neu.coe.csye7200.csv

import edu.neu.coe.csye7200.csv.tableParser.TableDatasetParser
import org.apache.spark.sql.{Dataset, SparkSession}

import scala.util.Try

/**
  * @author scalaprof
  */
object MovieDatabaseAnalyzer extends App {

  implicit val spark: SparkSession = SparkSession
    .builder()
    .appName("MovieDatabaseAnalyzer")
    .master("local[*]")
    .getOrCreate()

  spark.sparkContext.setLogLevel("ERROR") // We want to ignore all of the INFO and WARN messages.

  val movieTableParser: TableDatasetParser[Movie] = new TableDatasetParser[Movie] {}

  import MovieParser._
  import spark.implicits._

  val mdy: Try[Dataset[Movie]] = movieTableParser.parseResource("/movie_metadata.csv")
  mdy foreach {
    d =>
      println(d.count())
      d.show(10)
  }
}