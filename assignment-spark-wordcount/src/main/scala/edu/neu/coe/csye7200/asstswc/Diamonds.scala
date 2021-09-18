package edu.neu.coe.csye7200.asstswc

import org.apache.spark.sql.{DataFrame, SparkSession}

/**
 * @author Robin Hillyard
 */
object Diamonds extends App {
  val spark: SparkSession = SparkSession
    .builder()
    .appName("WordCount")
    .master("local[*]")
    .getOrCreate()
  spark.sparkContext.setLogLevel("ERROR")

  val diamonds: DataFrame = spark.read.format("csv")
          .option("header", "true")
          .option("inferSchema", "true")
          .load("assignment-spark-wordcount/diamonds.csv")

  diamonds.printSchema()
  diamonds.show()
}
