package edu.neu.coe.csye7200.assthw

import com.phasmidsoftware.flog.Flog

/**
 * A simple demonstration of functional logging with Flog.
 * See module 2.4 (Functional Programming in Scala), slides 27–28.
 *
 * The key idea: log values inside a for-comprehension without
 * breaking the functional style or extracting intermediate variables.
 */
object FlogExample:

  val flog: Flog = Flog[FlogExample.type]
  import flog.*

  // A simple transformation: parse a String as an Int, then double it
  def doubleIfValid(s: String): Option[Int] =
    s.toIntOption.map(_ * 2)

  // Without Flog, logging would force us to break this apart:
  //   for
  //     s <- inputs
  //     n <- doubleIfValid(s)
  //     _ = logger.log(s"doubleIfValid($s) = $n")
  //   yield n
  //
  // With Flog, we log each input value functionally, in-line:
  def processWithLogging(inputs: Seq[String]): Seq[Int] =
    for
      s <- inputs
      n <- s"doubleIfValid($s)" !! doubleIfValid(s)
    yield n

  @main def runFlogExample1(): Unit =
    val inputs = Seq("1", "two", "3", "four", "5")
    val results = processWithLogging(inputs)
    println(s"Results: $results")
// Expected output: Results: List(2, 6, 10)
// Flog will log each call to doubleIfValid, including the None cases

  @main def runFlogExample2(): Unit =
    val inputs = Seq("1", "two", "3", "four", "5")
    val output = inputs flatMap (x => s"doubleIfValid($x)" !! doubleIfValid(x))
    println(s"Output: $output")
  // Expected output: Output: List(2, 6, 10)
  // Flog will log each call to doubleIfValid, including the None cases
  // Without flog, we would have to write something like the following:

  @main def runNoFlogExample(): Unit =
    val inputs = Seq("1", "two", "3", "four", "5")
    val output = inputs flatMap {
      x =>
        val maybeInt = doubleIfValid(x)
        logger.info(s"doubleIfValid($x): $maybeInt")
        maybeInt
    }
    println(s"Output: $output")

