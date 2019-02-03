package edu.neu.coe.csye7200

import org.apache.spark.sql.SparkSession
import org.scalatest.tagobjects.Slow
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class WordCountSpark2ItSpec extends FlatSpec with Matchers with BeforeAndAfter  {

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
    WordCount.wordCount(spark.read.textFile(getClass.getResource("/WordCount.txt").getPath).rdd," ").collect() should matchPattern {
      case Array(("Hello",3),("World",3),("Hi",1)) =>
    }
  }

  it should "work for wordCount2" taggedAs Slow in {
    WordCount.wordCount2(spark.read.textFile(getClass.getResource("/WordCount2.txt").getPath).rdd," ").collect() should matchPattern {
      case Array(("hi",2), ("hello",1), ("and",1), ("world",2)) =>
    }
  }

  it should "work for wordCount3" taggedAs Slow in {
    WordCount.wordCount3(spark.read.textFile(getClass.getResource("/WordCount2.txt").getPath).rdd," ").collect() should matchPattern {
      case Array(("hi",2), ("hello",1), ("and",1), ("world",2)) =>
    }
  }

  it should "work for Dataset and Spark SQL" taggedAs Slow in {
    val ds = spark.read.textFile(getClass.getResource("/WordCount.txt").getPath)
    val words = WordCount.createWordDS(ds," ")
    words.createTempView("words")
    words.cache()
    spark.sql("select count(*) from words").head().getLong(0) shouldBe 7
    spark.sql("select word, count(*) from words group by word").collect().map(_.toString()) shouldBe
      Array("[World,3]","[Hi,1]","[Hello,3]")
  }

}
