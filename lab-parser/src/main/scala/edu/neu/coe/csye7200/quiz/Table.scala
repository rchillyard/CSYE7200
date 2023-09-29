package edu.neu.coe.csye7200.quiz

import scala.util.Random

case class Table[T](xs: List[T]) {
  def size: Int = xs.size

  def lensFilter[P](p: P=>Boolean)(lens: T=>P): Table[T] = /** SOLUTION */ Table(xs.filter(t => p(lens(t)))) /** SHOW ??? END */

  def filter(p: T => Boolean): Table[T] = /** SOLUTION */ lensFilter(p)(identity) /** SHOW ??? END */

  def sample(n: Int)(implicit r: Random): Table[T] = /** SOLUTION */ Table(xs.filter(_ => r.nextInt(n) == 0)) /** SHOW ??? END */
}

object Table {

  def apply[T](xs: Stream[T]): Table[T] = Table(xs.toList)

  def apply[T](xs: T*): Table[T] = Table(xs.toList)
}