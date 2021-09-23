package edu.neu.coe.csye7200.csv

import com.phasmidsoftware.table.Table
import org.apache.spark.sql.{Dataset, SparkSession}
import scala.util.Try

/**
 * @author scalaprof
 */
object MovieDatabaseAnalyzer extends App {

  val spark: SparkSession = SparkSession
          .builder()
          .appName("WordCount")
          .master("local[*]")
          .getOrCreate()

  spark.sparkContext.setLogLevel("ERROR") // We want to ignore all of the INFO and WARN messages.

  import MovieParser._
  import spark.implicits._

  val dy: Try[Dataset[Movie]] = Table.parseResource("/movie_metadata.csv") map (mt => spark.createDataset(mt.rows.toSeq))
  dy foreach {
    d =>
      println(d.count())
      d.show(10)
  }
}
