package edu.neu.coe.csye7200.asstswc

import org.apache.spark.rdd.RDD

object WordCount_foldByKey{
  //Use foldByKey to perform word count
  //hint: what is the key difference between reduceByKey and aggregateByKey, zeroValue!
  def wordCount(lines: RDD[String], separator: String): RDD[(String, Int)] = ??? // TO BE IMPLEMENTED

}
