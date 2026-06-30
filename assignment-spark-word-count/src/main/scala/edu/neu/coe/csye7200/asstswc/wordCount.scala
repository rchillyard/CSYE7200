package edu.neu.coe.csye7200.asstswc

import org.apache.spark.rdd.RDD
import org.apache.spark.sql.{Dataset, SparkSession}
import scala.util.{Failure, Success, Try}

/**
 * @author Yanda Yuan
 *         Compare different methods on different functions, which will be better on your project?
 *         wordCountSpec can be used as an example if you don't know what the result will be like.
 *         To help you understand more about the difference among functions: http://homepage.cs.latrobe.edu.au/zhe/ZhenHeSparkRDDAPIExamples.html
 */
object wordCount extends App { // TODO upgrade to Scala 3

  val methodMap: Map[Int, String] = Map(
    1 -> "aggregateByKey",
    2 -> "combineByKey",
    3 -> "countByValue",
    4 -> "foldByKey",
    5 -> "groupByKey",
    6 -> "reduceByKey",
    7 -> "ScalaVanilla")

  getDataset(getSparkSession, "WordCount01.txt") match {
    case Success(x) =>
      val n = 20
      val countMethod: Int = getCountMethod
      println(s"Result of word count with ${methodMap(countMethod)}:")
      getResult(x.rdd, countMethod) match {
        case Failure(exception) =>
          System.err.println(s"Error running word count: ${exception.getMessage}")
        case Success(Left(map)) =>
          map.take(n).foreach(println)
        case Success(Right(rdd)) =>
          rdd.take(n).foreach(println)
      }
    case Failure(exception) =>
      System.err.println(s"Error reading file: ${exception.getMessage}")
  }

  private def getSparkSession: SparkSession = {
    val spark: SparkSession = SparkSession
            .builder()
            .appName("WordCount")
            .master("local[*]")
            .getOrCreate()
    spark.sparkContext.setLogLevel("ERROR")
    spark
  }

  private def getDataset(sparkSession: SparkSession, filename: String): Try[Dataset[String]] =
    getPath(args.headOption.getOrElse(filename)) map {
      path => sparkSession.read.textFile(path)
    }

  private def getCountMethod: Int = {
    val prompt =
      """The count methods are:
        |  (1) aggregateByKey
        |  (2) combineByKey
        |  (3) countByValue
        |  (4) foldByKey
        |  (5) groupByKey
        |  (6) reduceByKey
        |  (7) ScalaVanilla""".stripMargin
    Console.println(prompt)
    Console.print("Enter the number of the method to run: ")
    val response = scala.io.StdIn.readLine()
    response.toInt
  }

  private def getResult(rdd: RDD[String], method: Int): Try[Either[collection.Map[String, Long], RDD[(String, Int)]]] =
    method match {
      case 1 =>
        Success(Right(WordCount_aggregateByKey.wordCount(trimmer(rdd), " ")))
      case 2 =>
        Success(Right(WordCount_combineByKey.wordCount(rdd, " ")))
      case 4 =>
        Success(Right(WordCount_foldByKey.wordCount(rdd, " ")))
      case 5 =>
        Success(Right(WordCount_groupByKey.wordCount(rdd, " ")))
      case 6 =>
        Success(Right(WordCount_reduceByKey.wordCount(rdd, " ")))
      case 3 =>
        Success(Left(WordCount_countByValue.wordCount(rdd, " ")))
      case 7 =>
        val list: List[String] = List("Hello World", "Hello World", "Hello World", "Hi")
        Success(Left(WordCount_ScalaVanilla.wordCount(list)))
      case _ =>
        Failure(new Exception("Not listed method, pls try again"))
    }

  //trim the input file: remove (some)special characters and capital letters
  private def trimmer(rdd: RDD[String]): RDD[String] =
    rdd.flatMap(_.split("\\s"))
            .map(_.replaceAll("[,.!?:;()%$'-]", "")
                    .trim
                    .toLowerCase)

  private def getPath(filename: String) = {
    val rAbsolute = """(/.*)""".r
    filename match {
      case rAbsolute(fullPath) =>
        Success(fullPath)
      case _ =>
        Try(getClass.getResource(filename)).map(_.getPath)
    }
  }
}
