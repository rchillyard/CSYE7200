/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200

object Junk3 extends App {

  def and(a: Boolean, b: () => Boolean): Boolean = if (a) b() else false

  def not(): Boolean = false

  println(and(a = true, not))
}
