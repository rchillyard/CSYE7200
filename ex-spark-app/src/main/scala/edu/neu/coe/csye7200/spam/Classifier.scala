package edu.neu.coe.csye7200.spam

import org.apache.spark.ml.classification.LogisticRegression
import org.apache.spark.ml.feature.HashingTF
import org.apache.spark.sql.SparkSession

object Classifier extends App {

  val spark = SparkSession.builder
          .appName("spam")
          .master("local[*]")
          .getOrCreate()

  import spark.implicits._

  val spam = spark.read.textFile("ex-spark-app/input/test/spam.txt")
  val norm = spark.read.textFile("ex-spark-app/input/test/normal.txt")

  val tf = new HashingTF().setNumFeatures(10000).setInputCol("words").setOutputCol("features")

  val spamDF = spam.map(email => (1.0, email.split(" ").toSeq)).toDF("label", "words")
  val normDF = norm.map(email => (0.0, email.split(" ").toSeq)).toDF("label", "words")
  val trainingData = tf.transform(spamDF.union(normDF)).cache()

  val model = new LogisticRegression().fit(trainingData)

  def predict(text: String): Double = {
    val row = tf.transform(Seq((0.0, text.split(" ").toSeq)).toDF("label", "words"))
    model.transform(row).select("prediction").head().getDouble(0)
  }

  println(s"Prediction for positive test example: ${predict("Subject: Cheap Stuff From: <omg.fu> O M G GET cheap stuff by sending money to Robin Hillyard")}")
  println(s"Prediction for negative test example: ${predict("Subject: Spark From: Robin Hillyard<scalaprof@gmail.com> Hi Adam, I started studying Spark the other day")}")

  spark.stop()
}