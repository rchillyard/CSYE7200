package edu.neu.coe.csye7200.asstswc

import org.apache.spark.sql.SparkSession
import org.scalatest.tagobjects.Slow
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class WordCountSpec extends FlatSpec with Matchers with BeforeAndAfter  {

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

  ignore should "work for wordCount" taggedAs Slow in {
    WordCount.wordCount(spark.read.textFile(getClass.getResource("WordCount.txt").getPath).rdd," ").collect() should matchPattern {
      case Array(("Hello",3),("World",3),("Hi",1)) =>
    }
  }

}
