package edu.neu.coe.csye7200.util

import org.slf4j

/**
  * Created by scalaprof on 5/9/17.
  */
object Logger_Cross {

  def getLogger(clazz: Class[_]): slf4j.Logger = org.slf4j.LoggerFactory.getLogger(clazz)

}
