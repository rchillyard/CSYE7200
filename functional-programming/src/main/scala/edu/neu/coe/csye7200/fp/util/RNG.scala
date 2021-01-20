package edu.neu.coe.csye7200.fp.util

import scala.util.Random

/**
  * Created by scalaprof on 6/1/16.
  */
class RNG[+A](f: Long => A)(seed: Long) {
  private val random = new Random(seed)
  private lazy val state = random.nextLong()

  def next = new RNG(f)(state)

  def value = f(state)
}

object RNG extends App {
  def modulo(n: Int, m: Int) = (n + m) % m

  val random = new Random(0L)
  val r = new RNG(x => modulo(x.toInt, 100))(0L)
  val someInt: Int = 55
  val r1 = r.next
  val r2 = r.next
  // ...
  val rN = r2.next
  val y = rN.value
  assert(y == someInt)
}
