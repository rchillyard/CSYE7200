package edu.neu.coe.csye7200.asstrs

import scala.util.Random

/**
  * Monadic trait which defines a random-state.
  *
  * Created by scalaprof on 9/24/16.
  *
  * @tparam T the underlying type of this random state, i.e. the type of the result of calling get
  */
trait RandomState[T] {
  /**
    * @return the next random state in the pseudo-random series
    */
  def next: RandomState[T]

  /**
    * @return the value of this random state
    */
  def get: T

  /**
    * Method to map this random state into another random state
    *
    * @param f the function to map a T value into a U value
    * @tparam U the underlying type of the resulting random state
    * @return a new random state
    */
  def map[U](f: T => U): RandomState[U]

  /**
    * Method to flatMap this random state into another random state
    *
    * @param f the function to map a T value into a RandomState[U] value
    * @tparam U the underlying type of the resulting random state
    * @return a new random state
    */
  // Hint: Think of the input and output, find the appropriate method that achieve this.
  // 10 points
  def flatMap[U](f: T => RandomState[U]): RandomState[U] = ??? // TO BE IMPLEMENTED

  /**
    * @return a stream of T values
    */
  // Hint: This a recursively method and it concatenate current element with following elements.
  // 12 points
  def toStream: LazyList[T] = ??? // TO BE IMPLEMENTED
}

/**
  * A concrete implementation of RandomState based on the Java random number generator
  *
  * @param n the random Long that characterizes this random state
  * @param g the function which maps a Long value into a T
  * @tparam T the underlying type of this random state, i.e. the type of the result of calling get
  */
case class JavaRandomState[T](n: Long, g: Long => T) extends RandomState[T] {
  // Hint: Remember to use the "seed" to generate next RandomState.
  // 7 points
  def next: RandomState[T] = ??? // TO BE IMPLEMENTED
  // Hint: Think of the input and output.
  // 5 points
  def get: T = ??? // TO BE IMPLEMENTED
  // Hint: This one need function composition.
  // 13 points
  def map[U](f: T => U): RandomState[U] = ??? // TO BE IMPLEMENTED
}

case class DoubleRandomState(n: Long) extends RandomState[Double] {
  val r = new Random(n)

  def next: RandomState[Double] = DoubleRandomState(r.nextLong())

  def get: Double = r.nextDouble()

  def map[U](f: Double => U): RandomState[U] = JavaRandomState[U](n, RandomState.longToDouble.andThen(f))
}

case class BetterRandomState[T](n: Long, h: Random => T) extends RandomState[T] {
  val r = new Random(n)

  def next: RandomState[T] = BetterRandomState(r.nextLong(), h)

  def get: T = h(r)

  def map[U](f: T => U): RandomState[U] = BetterRandomState[U](n, h andThen f)
}

object RandomState {
  def apply(n: Long): RandomState[Long] = JavaRandomState[Long](n, identity).next

  def apply(): RandomState[Long] = apply(System.currentTimeMillis)

  // Hint: This is a easy one, remember that it not only convert a Long to a Double but also scale down the number to -1 ~ 1.
  // 4 points
  val longToDouble: Long => Double = ??? // TO BE IMPLEMENTED
  val doubleToUniformDouble: Double => UniformDouble = { x => UniformDouble((x + 1) / 2) }
}

object BetterRandomState {
  val hDouble: Random => Double = { r => r.nextDouble() }
}

/**
  * This is essentially a wrapper of Double where (implicitly) 0 <= x <= 1.
  * Note that we would like to specify it as a Value type but require statements are not legal in Value types
  */
case class UniformDouble(x: Double) extends AnyVal {
  //  if(x<0.0 || x>1.0) throw new RuntimeException(s"UniformDouble may not be outside range 0->1: $x")
  def +(y: Double): UniformDouble = {
    val z = x + y
    if (z >= 0.0 && z <= 1.0)
      new UniformDouble(z)
    else throw new RuntimeException(s"UniformDouble may not be outside range 0->1: $z")
  }
}

object UniformDouble {
  def apply(x: Double): UniformDouble = new UniformDouble(x) + 0
}

object UniformDoubleRandomState
