package edu.neu.coe.csye7200.asstswc

import org.apache.spark.rdd.RDD

object WordCount_countByValue {
  //Use countByValue to perform word count
  //hint: check the data type of output, why it is not RDD
  def wordCount(lines: RDD[String], separator: String): collection.Map[String, Long] = ??? // TO BE IMPLEMENTED

}
