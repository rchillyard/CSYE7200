package edu.neu.coe.csye7200.asstswc

import org.apache.spark.sql.SparkSession
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow
import org.scalatest.{BeforeAndAfter, flatspec}
import scala.util.Try

class WordCountSpec extends flatspec.AnyFlatSpec with Matchers with BeforeAndAfter  {

  implicit var spark: SparkSession = _

  before {
    spark = SparkSession
      .builder()
      .appName("WordCount")
      .master("local[*]")
      .getOrCreate()
  }

  after {
    if (spark != null) {
      spark.stop()
    }
  }

  behavior of "Spark"

  it should "work for wordCount" taggedAs Slow in {
    val triedPath = Try(getClass.getResource("WordCount.txt").getPath)
    triedPath.isSuccess shouldBe true
    for (path <- triedPath)
    WordCount.wordCount(spark.read.textFile(path).rdd," ").collect() should matchPattern {
      case Array(("Hello",3),("World",3),("Hi",1)) =>
    }
  }

}
