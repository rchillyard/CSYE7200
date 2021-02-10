package edu.neu.coe.csye7200.asstswc

import org.apache.spark.sql.SparkSession
import scala.util.{Success, Try}
import org.apache.spark.rdd.RDD

/**
 * @author Yanda Yuan
 * Compare different methods on different functions, which will be better on your project?
 * wordCountSpec can be used as an example if you don't know what the result will be like.
 * To help you understand more about the difference among functions: http://homepage.cs.latrobe.edu.au/zhe/ZhenHeSparkRDDAPIExamples.html
 */
object wordCount extends App {
  val list: List[String] = List("Hello World", "Hello World", "Hello World", "Hi")
  println("Enter method to run")
  println("  (aggregateByKey, combineByKey, countByValue, foldByKey, groupByKey, reduceByKey, ScalaVanilla):")
  val countMethod = scala.io.StdIn.readLine()

  val spark: SparkSession = SparkSession
    .builder()
    .appName("WordCount")
    .master("local[*]")
    .getOrCreate()
  spark.sparkContext.setLogLevel("ERROR")
  val rAbsolute = """(/.*)""".r
  val filename = args.headOption.getOrElse("WordCount01.txt")
  val path: String = filename match {
    case rAbsolute(fullPath) => fullPath
    case _ =>
      (for (uo <- Try(Option(getClass.getResource(filename)))) yield for (u <- uo) yield u) match {
        case Success(Some(p)) => p.getPath
        case _ => throw new Exception(s"cannot get resource for class WordCount: $filename")
      }
  }
  //trim the input file: remove (some)special characters and capital letters
  def trimer(rdd: RDD[String]): RDD[String] = {
    rdd.flatMap(_.split("\\s"))
      .map(_.replaceAll("[,.!?:;()%$'-]", "")
        .trim
        .toLowerCase)
  }

  println(s"Result of word count with $countMethod:")
  countMethod match {
    case "aggregateByKey" => WordCount_aggregateByKey.wordCount(trimer(spark.read.textFile(path).rdd), " ").take(20) foreach println
    case "combineByKey" => WordCount_combineByKey.wordCount(spark.read.textFile(path).rdd, " ").take(20) foreach println
    case "countByValue" => WordCount_countByValue.wordCount(spark.read.textFile(path).rdd, " ").take(20) foreach println
    case "foldByKey" => WordCount_foldByKey.wordCount(spark.read.textFile(path).rdd, " ").take(20) foreach println
    case "groupByKey" => WordCount_groupByKey.wordCount(spark.read.textFile(path).rdd, " ").take(20) foreach println
    case "reduceByKey" => WordCount_reduceByKey.wordCount(spark.read.textFile(path).rdd, " ").take(20) foreach println
    case "ScalaVanilla" => WordCount_ScalaVanilla.wordCount(list) foreach println
    case _ => println("Not listed method, pls try again")
  }

}
