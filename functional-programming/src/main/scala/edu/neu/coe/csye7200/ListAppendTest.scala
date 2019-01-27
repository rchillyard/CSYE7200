package edu.neu.coe.csye7200

import scala.collection.mutable.ListBuffer

/**
  * Created by scalaprof on 9/19/16.
  */
object ListAppendTest extends App {

  val propertyData = List(""""spark.shuffle.memoryFraction"="0.5"""", """"spark.yarn.executor.memoryOverhead"="712" """)

  val propertyList = new ListBuffer[(String, String)]()

  propertyData.foreach { line =>
    val c = line.split("=")
    propertyList.append((c(0), c(1)))
  }

  println(propertyList)
}
