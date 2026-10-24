package edu.neu.coe.csye7200.cache

import scala.util.Random

object MockStock {

  private val random = Random

  def lookupStock(k: String): Double = {
    random.setSeed(k.hashCode)
    random.nextInt(1000) / 100.0
  }

}
