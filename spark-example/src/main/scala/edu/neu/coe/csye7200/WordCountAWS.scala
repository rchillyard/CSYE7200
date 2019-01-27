package edu.neu.coe.csye7200

import org.apache.spark.sql.SparkSession

/**
  * This is an example for running spark application WordCount in AWS EMR
  * Usage:
  * 1. Use sbt package to generate a jar file and upload it to your AWS S3
  * 2. Create an Cluster in AWS EMR including at least Spark 2.2.0 or higher
  * 3. In your Cluster -> Steps tab, add a new Spark application step
  * 4. Spark-submit options: --class edu.neu.csye._7200.WordCountAWS
  * 5. Application location: find your jar in your AWS S3
  * 6. Arguments: <input> <output>
  *    This example take 2 arguments, input path and output path
  *    Example: s3://yourbucketname/input/WordCount.txt s3://yourbucketname/outputfolder
  *    You may find test file under input folder in this repo
  */
object WordCountAWS {

  def main(args: Array[String]): Unit = {

    if (args.length != 2) {
      println("input arguments: <input> <output>")
      System.exit(2)
    }

    val in = args.head
    val out = args(1)

    implicit val spark = SparkSession
      .builder()
      .appName("WordCountAWS")
      //.master("local[*]") //Uncomment this line if you want to test in local
      .getOrCreate()

    //repartition(1) will send all data to one node therefore it is not recommended for large dataset, this is only for easy reading.
    WordCount.wordCount(spark.read.textFile(in).rdd," ").repartition(1).saveAsTextFile(out)

    val words = WordCount.createWordDS(spark.read.textFile(in)," ")
    words.createTempView("words")
    words.cache()
    //repartition(1) will send all data to one node therefore it is not recommended for large dataset, this is only for easy reading.
    spark.sql("select word, count(*) from words group by word").repartition(1).write.mode("append").csv(out+"sql")

  }

}
