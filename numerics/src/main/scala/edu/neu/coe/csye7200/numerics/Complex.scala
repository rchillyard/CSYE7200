package edu.neu.coe.csye7200.numerics

import scala.util.parsing.combinator.JavaTokenParsers

case class Complex(real: Double, imag: Double) {
  val sumSquares = real * real + imag * imag

  import Complex.ComplexHasImaginary._

  def +(c: Complex): Complex = plus(this, c)

  def *(c: Complex): Complex = times(this, c)

  def -(c: Complex): Complex = minus(this, c)

  def /(c: Complex): Complex = div(this, c)

  def inverse: Complex = Complex(real / sumSquares, -imag / sumSquares)

  def modulus: Double = math.sqrt(sumSquares)

  def polar: (Double, Double) = (modulus, math.atan2(imag, real))

  def moveHorizontal(dx: Double): Complex = copy(real = real + dx)

  def moveVertical(dy: Double): Complex = copy(imag = imag + dy)
}

object Complex {
  val i: Complex = Complex(0, 1)
  val zero: Complex = apply(0)
  val one: Complex = apply(1)
  val minusOne: Complex = apply(-1)

  /**
   * Construct a Complex which has real part x and imaginary part 0.
   *
   * @param x the real part.
   * @return a new Complex.
   */
  def apply(x: Double): Complex =
  // TO BE IMPLEMENTED
  ???

  /**
   * Construct a Complex from polar coordinates (z).
   *
   * @param z polar coordinates.
   * @return a Complex.
   */
  def create(z: (Double, Double)): Complex = Complex(z._1 * math.cos(z._2), z._1 * math.sin(z._2))

  val parser: ComplexParser = new ComplexParser()

  trait ComplexHasImaginary extends HasImaginary[Complex] {

    def div(x: Complex, y: Complex): Complex = times(x, y.inverse)

    def plus(x: Complex, y: Complex): Complex =
    // TO BE IMPLEMENTED
    ???

    def minus(x: Complex, y: Complex): Complex = plus(x, negate(y))

    def times(x: Complex, y: Complex): Complex =
    // TO BE IMPLEMENTED
    ???

    def negate(x: Complex): Complex = times(x, minusOne)

    def fromInt(x: Int): Complex = Complex(x)

    def parseString(str: String): Option[Complex] = {
      parser.parseAll(parser.complex, str) match {
        case parser.Success(x,_) => Some(x.asInstanceOf[Complex])
        case parser.Failure(_, _) => None
        case parser.Error(_, _) => None
      }
    }

    def conjugate(t: Complex): Complex = t.copy(imag = -t.imag)

    def toInt(x: Complex): Int = throw new UnsupportedOperationException

    def toLong(x: Complex): Long = throw new UnsupportedOperationException

    def toFloat(x: Complex): Float = throw new UnsupportedOperationException

    def toDouble(x: Complex): Double = throw new UnsupportedOperationException

    def compare(x: Complex, y: Complex): Int = throw new UnsupportedOperationException
  }

  implicit object ComplexHasImaginary extends ComplexHasImaginary

}

class ComplexParser extends JavaTokenParsers {

  def complex: Parser[Complex] = floatingPointNumber ~ opt("i" ~> floatingPointNumber) ^^ {
    case r ~ io => Complex(r.toDouble, io.map(_.toDouble).getOrElse(0))
  }
}

trait HasImaginary[T] extends Fractional[T] {
  def conjugate(t: T): T
}
