package edu.neu.coe.csye7200.asstswc

import org.apache.spark.sql.SparkSession
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow
import org.scalatest.{BeforeAndAfter, flatspec}
import scala.util.Try

/**
 * @author Yanda Yuan
 */
class WordCountReduceByKeySpec extends flatspec.AnyFlatSpec with Matchers with BeforeAndAfter  {

  implicit var spark: SparkSession = _

  before {
    spark = SparkSession
      .builder()
      .appName("WordCount")
      .master("local[*]")
      .getOrCreate()
    spark.sparkContext.setLogLevel("ERROR")
  }

  after {
    if (spark != null) {
      spark.stop()
    }
  }

  behavior of "Spark"

  it should "aggregateByKey work for wordCount" taggedAs Slow in {
    val triedPath = Try(getClass.getResource("WordCount.txt").getPath)
    triedPath.isSuccess shouldBe true
    for (path <- triedPath)
    WordCount_aggregateByKey.wordCount(spark.read.textFile(path).rdd, " ").collect() should matchPattern {
      case Array(("Hello", 3), ("World", 3), ("Hi", 1)) =>
    }
  }
  it should "combineByKey work for wordCount" taggedAs Slow in {
    val triedPath = Try(getClass.getResource("WordCount.txt").getPath)
    triedPath.isSuccess shouldBe true
    for (path <- triedPath)
      WordCount_combineByKey.wordCount(spark.read.textFile(path).rdd," ").collect() should matchPattern {
        case Array(("Hello",3),("World",3),("Hi",1)) =>
      }
  }
  it should "countByValue work for wordCount" taggedAs Slow in {
    val expected = Map("Hello" -> 3, "World" -> 3, "Hi" -> 1)
    val triedPath = Try(getClass.getResource("WordCount.txt").getPath)
    triedPath.isSuccess shouldBe true
    for (path <- triedPath) {
      val actual = WordCount_countByValue.wordCount(spark.read.textFile(path).rdd, " ")
      expected.equals(actual) shouldBe true
    }
  }
  it should "foldByKey work for wordCount" taggedAs Slow in {
    val triedPath = Try(getClass.getResource("WordCount.txt").getPath)
    triedPath.isSuccess shouldBe true
    for (path <- triedPath)
      WordCount_foldByKey.wordCount(spark.read.textFile(path).rdd," ").collect() should matchPattern {
        case Array(("Hello",3),("World",3),("Hi",1)) =>
      }
  }
  it should "groupByKey work for wordCount" taggedAs Slow in {
    val triedPath = Try(getClass.getResource("WordCount.txt").getPath)
    triedPath.isSuccess shouldBe true
    for (path <- triedPath)
      WordCount_groupByKey.wordCount(spark.read.textFile(path).rdd," ").collect() should matchPattern {
        case Array(("Hello",3),("World",3),("Hi",1)) =>
      }
  }
  it should "reduceByKey work for wordCount" taggedAs Slow in {
    val triedPath = Try(getClass.getResource("WordCount.txt").getPath)
    triedPath.isSuccess shouldBe true
    for (path <- triedPath)
      WordCount_reduceByKey.wordCount(spark.read.textFile(path).rdd," ").collect() should matchPattern {
        case Array(("Hello",3),("World",3),("Hi",1)) =>
      }
  }
  it should "ScalaVanilla work for wordCount" taggedAs Slow in {
    val inputList: List[String] = List("Hello World", "Hello World", "Hello World", "Hi")
    val expected = Map("Hello" -> 3, "World" -> 3, "Hi" -> 1)
    val triedPath = Try(getClass.getResource("WordCount.txt").getPath)
    triedPath.isSuccess shouldBe true
    for (path <- triedPath) {
      val actual = WordCount_ScalaVanilla.wordCount(inputList)
      expected.equals(actual) shouldBe true
    }
  }
}
