package edu.neu.coe.csye7200.asstswc

import org.apache.spark.rdd.RDD

object WordCount_aggregateByKey{
  //Use aggregateByKey to perform word count
  //hint: what is the key difference between foldByKey and aggregateByKey?
  def wordCount(lines: RDD[String], separator: String): RDD[(String, Int)] = ??? // TO BE IMPLEMENTED

}
