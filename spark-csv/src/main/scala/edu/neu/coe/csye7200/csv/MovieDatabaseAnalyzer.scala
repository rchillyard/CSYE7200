package edu.neu.coe.csye7200.csv

import com.phasmidsoftware.table.Table
import org.apache.spark.sql.{Dataset, SparkSession}
import scala.util.Try


/**
 * @author scalaprof
 */
case class MovieDatabaseAnalyzer(resource: String) {

  val spark: SparkSession = SparkSession
          .builder()
          .appName("WordCount")
          .master("local[*]")
          .getOrCreate()

  spark.sparkContext.setLogLevel("ERROR") // We want to ignore all of the INFO and WARN messages.

  import MovieParser._
  import spark.implicits._

  private val mty: Try[Table[Movie]] = Table.parseResource(resource, getClass)
  val dy: Try[Dataset[Movie]] = mty map {
    mt =>
      println(s"Movie table has ${mt.size} rows")
      spark.createDataset(mt.rows.toSeq)

  }
}


/**
 * @author scalaprof
 */
object MovieDatabaseAnalyzer extends App {

  def apply(resource: String): MovieDatabaseAnalyzer = new MovieDatabaseAnalyzer(resource)

  apply("/movie_metadata.csv").dy foreach {
    d =>
      println(d.count())
      d.show(10)
  }
}
