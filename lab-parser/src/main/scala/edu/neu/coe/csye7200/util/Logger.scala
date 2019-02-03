/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.util

/**
  * Created by scalaprof on 5/9/17.
  */
case class Logger(log: org.slf4j.Logger) {
  // Log methods that take only a String
  def logInfo(msg: => String) {
    if (log.isInfoEnabled) log.info(msg)
  }

  def logDebug(msg: => String) {
    if (log.isDebugEnabled) log.debug(msg)
  }

  def logTrace(msg: => String) {
    if (log.isTraceEnabled) log.trace(msg)
  }

  def logWarning(msg: => String) {
    if (log.isWarnEnabled) log.warn(msg)
  }

  def logError(msg: => String) {
    if (log.isErrorEnabled) log.error(msg)
  }

  // Log methods that take Throwables (Exceptions/Errors) too
  def logInfo(msg: => String, throwable: Throwable) {
    if (log.isInfoEnabled) log.info(msg, throwable)
  }

  def logDebug(msg: => String, throwable: Throwable) {
    if (log.isDebugEnabled) log.debug(msg, throwable)
  }

  def logTrace(msg: => String, throwable: Throwable) {
    if (log.isTraceEnabled) log.trace(msg, throwable)
  }

  def logWarning(msg: => String, throwable: Throwable) {
    if (log.isWarnEnabled) log.warn(msg, throwable)
  }

  def logError(msg: => String, throwable: Throwable) {
    if (log.isErrorEnabled) log.error(msg, throwable)
  }

  def isTraceEnabled: Boolean = {
    log.isTraceEnabled
  }

}

object Logger {

  def apply(clazz: Class[_]): Logger = apply(Logger_Cross.getLogger(clazz))

}
