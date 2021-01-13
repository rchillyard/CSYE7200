package edu.neu.coe.csye7200.asstswc

import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession

/***
  * This is an example of spark word count.
  * Replace the ??? with appropriate implementation and run this file
  * with the argument "input/ScalaWiki.txt"
  * Write down the count of word "Scala" in your submission
  * You can find the implementation from https://spark.apache.org/examples.html
  */

object WordCount extends App {

  def wordCount(lines: RDD[String], separator: String): RDD[(String, Int)] = {
    lines.flatMap(_.split(separator))
      .map((_, 1))
      .reduceByKey(_ + _)
      .sortBy(-_._2)
  }

  override def main(args: Array[String]) = {

    val spark: SparkSession = SparkSession
      .builder()
      .appName("WordCount")
      .master("local[*]")
      .getOrCreate()

    if (args.length>0) {
      wordCount(spark.read.textFile(args.head).rdd," ").take(10).foreach(println(_))
    }
  }

}
