package edu.neu.coe.csye7200.spam

import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.feature.HashingTF
import org.apache.spark.mllib.classification.LogisticRegressionWithSGD
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext

/**
 * @author scalaprof
 */
object Classifier extends App {
  
  val conf = new SparkConf().setAppName("spam").setMaster("local[*]")
  val sc = new SparkContext(conf)
  val spam = sc.textFile("spark-app//input//test//spam.txt")
  val norm = sc.textFile("spark-app//input//test//normal.txt")

  val tf = new HashingTF(10000)
  val spamFeatures = spam.map(email => tf.transform(email.split(" ")))
  val normFeatures = norm.map(email => tf.transform(email.split(" ")))
  
  val posExamples = spamFeatures.map(f => LabeledPoint(1, f))
  val negExamples = normFeatures.map(f => LabeledPoint(0, f))
  val trainingData = posExamples.union(negExamples)
  trainingData.cache()
  
  val model = new LogisticRegressionWithSGD().run(trainingData)
  
  val posTest = tf.transform("Subject: Cheap Stuff From: <omg.fu> O M G GET cheap stuff by sending money to Robin Hillyard".split(" "))
  val negTest = tf.transform("Subject: Spark From: Robin Hillyard<scalaprof@gmail.com> Hi Adam, I started studying Spark the other day".split(" "))
  
  println(s"Prediction for positive test example: ${model.predict(posTest)}")
  println(s"Prediction for negative test example: ${model.predict(negTest)}")
}
