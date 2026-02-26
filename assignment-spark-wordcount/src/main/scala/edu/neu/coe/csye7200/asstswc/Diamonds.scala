/**
 * @author Robin Hillyard
 */
package edu.neu.coe.csye7200.asstswc

import org.apache.spark.sql.{DataFrame, SparkSession}

object Diamonds extends App {
  val spark: SparkSession = SparkSession
          .builder()
          .appName("WordCount")
          .master("local[*]")
//          .config("spark.driver.extraJavaOptions", "--add-opens java.base/sun.nio.ch=ALL-UNNAMED")
//          .config("spark.executor.extraJavaOptions", "--add-opens java.base/sun.nio.ch=ALL-UNNAMED")
          .getOrCreate()
  spark.sparkContext.setLogLevel("ERROR")

  val diamonds: DataFrame = spark.read.format("csv")
          .option("header", "true")
          .option("inferSchema", "true")
          .load("assignment-spark-wordcount/diamonds.csv")

  diamonds.printSchema()
  if (diamonds.count() == 0) throw new Exception("No diamonds found")
  diamonds.show()

  spark.stop()
}