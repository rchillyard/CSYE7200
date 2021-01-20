package edu.neu.coe.csye7200.asstswc

import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession
import scala.util.{Success, Try}

/** *
  * This is an example of spark word count.
  * Provide a filename on the command line (Program Arguments).
  * with the argument "input/ScalaWiki.txt"
  * Write down the count of word "Scala" in your submission
  * You can find the implementation from https://spark.apache.org/examples.html
  */

object WordCount extends App {

  def wordCount(lines: RDD[String], separator: String): RDD[(String, Int)] =
    lines.flatMap(_.split(separator))
            .map((_, 1))
            .reduceByKey(_ + _)
            .sortBy(-_._2)


  val spark: SparkSession = SparkSession
          .builder()
          .appName("WordCount")
          .master("local[*]")
          .getOrCreate()

  val rAbsolute = """(/.*)""".r
  val filename = args.headOption.getOrElse("WordCount.txt")
  val path: String = filename match {
    case rAbsolute(fullPath) => fullPath
    case _ =>
      (for (uo <- Try(Option(getClass.getResource(filename)))) yield for (u <- uo) yield u) match {
        case Success(Some(p)) => p.getPath
        case _ => throw new Exception(s"cannot get resource for class WordCount: $filename")
      }
  }
  wordCount(spark.read.textFile(path).rdd, " ").take(10) foreach println

}
