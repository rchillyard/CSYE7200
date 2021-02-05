package edu.neu.coe.csye7200.asstswc

import org.apache.spark.rdd.RDD

object WordCount_combineByKey{
  //Use combineByKey to perform word count
  //To help you understand, check Spark doc or links in workCount
  def wordCount(lines: RDD[String], separator: String): RDD[(String, Int)] = ??? // TO BE IMPLEMENTED

}
