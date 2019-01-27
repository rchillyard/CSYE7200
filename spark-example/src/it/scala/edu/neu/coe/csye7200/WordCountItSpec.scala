package edu.neu.coe.csye7200

import org.apache.spark.{SparkConf, SparkContext}
import org.scalatest.tagobjects.Slow
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class WordCountItSpec extends FlatSpec with Matchers with BeforeAndAfter  {

  private var sc: SparkContext = _

  before {
    sc = new SparkContext(new SparkConf().setAppName("WordCount").setMaster("local[*]"))
  }

  after {
    if (sc != null) {
      sc.stop()
    }
  }

  "result" should "right for wordCount" taggedAs Slow in {
    WordCount.wordCount(sc.textFile(getClass.getResource("/WordCount.txt").getPath)," ").collect() should matchPattern {
      case Array(("Hello",3),("World",3),("Hi",1)) =>
    }
  }
}
