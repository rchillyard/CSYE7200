package edu.neu.coe.csye7200.fp.exam

import scala.util.Try

case class Complex(real: Double, imag: Double)

trait WaveFunction extends ((Double, Double) => Complex) {
  def apply(x: Double, t: Double): Complex

  def +(w: WaveFunction): WaveFunction

  def *(s: Double): WaveFunction
}

case class SuperPosition(xs: List[(Double, WaveFunction)]) extends (() => WaveFunction) {
  def apply(): WaveFunction = ??? // TO BE IMPLEMENTED
}

case object Zero extends WaveFunction {
  def apply(x: Double, t: Double): Complex = Complex(0, 0)

  def +(w: WaveFunction): WaveFunction = w

  def *(s: Double): WaveFunction = Zero

}

case class SquareWellWaveFunction(l: Double) extends WaveFunction {
  def apply(x: Double, t: Double): Complex = Complex(0, 0)

  def +(w: WaveFunction): WaveFunction = SquareWellWaveFunction(l)

  def *(s: Double): WaveFunction = SquareWellWaveFunction(l)

  override def toString(): String = s"SquareWellWaveFunction($l)"
}

object WaveFunctionTest extends App {
  val sp = SuperPosition(List(2.0 -> SquareWellWaveFunction(1), 1.0 -> SquareWellWaveFunction(2)))
  println(sp())

  def getLiftedScaleFunction(w: WaveFunction): Double => WaveFunction = ??? // TO BE IMPLEMENTED
  val f: Double => WaveFunction = getLiftedScaleFunction(SquareWellWaveFunction(1))
  val fLift: Try[Double] => Try[WaveFunction] = ??? // TO BE IMPLEMENTED
  val wy = fLift(Try(1.0))
  println(wy.get)
}
