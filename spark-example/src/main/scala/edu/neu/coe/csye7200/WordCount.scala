package edu.neu.coe.csye7200

import org.apache.spark.rdd.RDD
import org.apache.spark.sql.{Dataset, SparkSession}
import org.apache.spark.{SparkConf, SparkContext}

/**
  * FIXME this doesn;t work
  */
object WordCount extends App {

  def wordCount(lines: RDD[String],separator: String) = {
    lines.flatMap(_.split(separator))
         .map((_,1))
         .reduceByKey(_ + _)
  }

  def wordCount2(lines: RDD[String], separator: String) = {
    lines.flatMap(_.split(separator))
        .filter(!_.contains("He"))
        .map(_.replace(",", ""))
        .map((_,1))
        .reduceByKey(_ + _)
  }

  def wordCount3(lines: RDD[String], separator: String) = {
    lines.flatMap(_.split(separator))
        .filter(myFilter(_, "He"))
        .map(myReplacer)
        .map((_,1))
        .reduceByKey(_ + _)
  }

  def myFilter(input: String, keyword: String) = !input.contains(keyword)

  def myReplacer(input: String) = input.replace(",","")

  case class Word(word: String, count: Int)

  def createWordDS(ds: Dataset[String], separator: String)(implicit spark:SparkSession) = {
    import spark.implicits._
    ds.flatMap(_.split(separator))
      .map((_,1))
      .map(Word.tupled)
      .as[Word]
  }

  //For Spark 1.0-1.9
  val sc = new SparkContext(new SparkConf().setAppName("WordCount").setMaster("local[*]"))

  wordCount(sc.textFile("test//resources//WordCount.txt")," ").collect().foreach(println(_))

  sc.stop()

  //For Spark 2.0+
  implicit val spark = SparkSession
    .builder()
    .appName("WordCount")
    .master("local[*]")
    .getOrCreate()

  wordCount(spark.read.textFile("test//resources//WordCount.txt").rdd," ").collect().foreach(println(_))

  //Spark SQL example
  val wordDS = createWordDS(spark.read.textFile("test//resources//WordCount.txt")," ")

  wordDS.createTempView("words")
  wordDS.cache()

  spark.sql("select word, count(*) from words group by word").show(10)

  spark.stop()
}
