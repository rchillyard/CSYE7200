package edu.neu.coe.csye7200.csv

import com.phasmidsoftware.table.Table
import org.apache.spark.sql.{DataFrame, Dataset, SparkSession, functions}

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


  val movieData = spark.read.option("delimiter", ",")
    .option("header", "true")
    .csv(resource)

  def mean() = {
    val mean = movieData.select(functions.avg("imdb_score"))
    mean.show()
    mean.collect()(0).get(0)
  }

  def standardDeviation(): Any = {
    val stdv = movieData.select(functions.stddev("imdb_score"))
    stdv.show()
    stdv.collect()(0).get(0)
  }

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

 // To answer where I got this file : C:\Users\c_jas\CSYE7200\CSYE7200\spark-csv\src\main\resources\movie_metadata.csv
  apply("/movie_metadata.csv").mean()
  apply("/movie_metadata.csv").standardDeviation()
}

