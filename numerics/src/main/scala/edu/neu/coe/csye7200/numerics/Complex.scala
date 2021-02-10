package edu.neu.coe.csye7200.numerics


case class Complex(real: Double, imag: Double) {
  val sumSquares = real * real + imag * imag

  import Complex.ComplexIsFractional._

  def +(c: Complex): Complex = plus(this, c)

  def *(c: Complex): Complex = times(this, c)

  def -(c: Complex): Complex = minus(this, c)

  def /(c: Complex): Complex = div(this, c)

  def inverse: Complex = Complex(real / sumSquares, -imag / sumSquares)

  def modulus: Double = math.sqrt(sumSquares)

  def polar: (Double, Double) = (modulus, math.atan2(imag, real))
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
  def apply(x: Double): Complex = ???

  /**
   * Construct a Complex from polar coordinates (z).
   *
   * @param z polar coordinates.
   * @return a Complex.
   */
  def create(z: (Double, Double)): Complex = Complex(z._1 * math.cos(z._2), z._1 * math.sin(z._2))

  trait ComplexIsFractional extends Fractional[Complex] {

    def div(x: Complex, y: Complex): Complex = times(x, y.inverse)

    def plus(x: Complex, y: Complex): Complex = ???

    def minus(x: Complex, y: Complex): Complex = plus(x, negate(y))

    def times(x: Complex, y: Complex): Complex = ???

    def negate(x: Complex): Complex = times(x, minusOne)

    def fromInt(x: Int): Complex = Complex(x)

    def parseString(str: String): Option[Complex] = ???

    def toInt(x: Complex): Int = throw new UnsupportedOperationException

    def toLong(x: Complex): Long = throw new UnsupportedOperationException

    def toFloat(x: Complex): Float = throw new UnsupportedOperationException

    def toDouble(x: Complex): Double = throw new UnsupportedOperationException

    def compare(x: Complex, y: Complex): Int = throw new UnsupportedOperationException
  }

  implicit object ComplexIsFractional extends ComplexIsFractional

}

