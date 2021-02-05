package edu.neu.coe.csye7200.asstswc

import org.apache.spark.rdd.RDD

/** *
  * This is an example of spark word count using reduceByKey.
  * You can find the implementation from https://spark.apache.org/examples.html
  */

object WordCount_reduceByKey{

  def wordCount(lines: RDD[String], separator: String): RDD[(String, Int)] =
    lines.flatMap(_.split(separator))
            .map((_, 1))
            .reduceByKey(_ + _)
            .sortBy(-_._2)

}
