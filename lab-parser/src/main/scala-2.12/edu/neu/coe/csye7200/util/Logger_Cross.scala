/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.util

import org.slf4j

/**
  * Created by scalaprof on 5/9/17.
  */
object Logger_Cross {

  /**
    * TODO reimplement using scala-logging
    *
    * @param clazz the class for which we want a logger
    * @return a logger
    */
  def getLogger(clazz: Class[_]): slf4j.Logger = org.slf4j.LoggerFactory.getLogger(clazz)

}
