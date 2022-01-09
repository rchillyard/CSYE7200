package edu.neu.coe.csye7200.numerics

/**
  * This is support for PowerPoint 4.0 Functional Composition.
  */
object TemperatureConverter extends App {

  def fToC(x: Double): Double = (x - 32) * 5 / 9

  def cToF(x: Double): Double = x * 9 / 5 + 32

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def convert(x: String)(f: Double => Double, w: String): String =
    lift[Double, Double](f)(x.toDoubleOption) map (c => c.toString + w) getOrElse "invalid input"

  def fToCs(x: String): String = convert(x)(fToC, "C")

  def cToFs(x: String): String = convert(x)(cToF, "F")

  val fc = args.isEmpty || args.contains("F")
  val scanner = new java.util.Scanner(System.in)
  System.err.print(s"Temperature in ${if (fc) "Fahrenheit" else "Celsius"}? ")
  val x = scanner.nextLine()
  val f = if (fc) fToCs _ else cToFs _
  println(f(x))
}
