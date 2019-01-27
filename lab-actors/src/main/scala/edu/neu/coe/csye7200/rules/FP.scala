/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.rules

import scala.util.{Failure, Success, Try}

/**
  * Please see LaScala for the originals
  */
object FP {

  def map2[T1, T2, R](t1o: Option[T1], t2o: => Option[T2])(f: (T1, T2) => R): Option[R] = for {t1 <- t1o; t2 <- t2o} yield f(t1, t2)

  def map2[T1, T2, R](t1y: Try[T1], t2y: Try[T2])(f: (T1, T2) => R): Try[R] = for {t1 <- t1y; t2 <- t2y} yield f(t1, t2)

  def optionToTry[X](xo: Option[X], t: => Throwable): Try[X] = {
    xo match {
      case Some(x) => Success(x)
      case None => if (t != null) Failure(t) else Failure(new java.util.NoSuchElementException)
    }
  }

  def optionToTry[X](xo: Option[X]): Try[X] = optionToTry(xo, null)


}
