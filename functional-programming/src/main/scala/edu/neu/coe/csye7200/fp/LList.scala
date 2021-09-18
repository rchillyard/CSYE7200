package edu.neu.coe.csye7200

/**
  * Created by scalaprof on 10/31/16.
  */
trait LList[+T] {
  def head: T

  def tail: LList[T]

  def isEmpty: Boolean

  def iterator: Iterator[T]
}

case class LCons[+T](head: T, tail: LList[T]) extends LList[T] {
  //noinspection NotImplementedCode
  def iterator: Iterator[T] = ???

  def isEmpty: Boolean = false
}

case object MT extends LList[Nothing] {
  def head: Nothing = throw new Exception("cannot get head from MT")

  def tail: LList[Nothing] = throw new Exception("cannot get tail from MT")

  //noinspection NotImplementedCode
  def iterator: Iterator[Nothing] = ???

  def isEmpty: Boolean = true
}
