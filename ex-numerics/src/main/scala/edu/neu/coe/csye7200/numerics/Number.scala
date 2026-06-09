package edu.neu.coe.csye7200.numerics

import edu.neu.coe.csye7200.parse.NumberParser
import java.util.NoSuchElementException
import scala.util.*

/**
 * This class is designed to model a Numerical value of various possible different types.
 * These types are: Int, BigInt, Rational, Double.
 *
 * @param value  the value of the Number, expressed as a nested Either type.
 * @param factor the scale factor of the Number: valid scales are: Scalar, Pi, and E.
 */
case class Number(value: Either[Either[Either[Option[Double], Rational], BigInt], Int], factor: Factor) {

  /**
   * Auxiliary constructor for the usual situation with the default factor.
   *
   * @param v the value for the new Number.
   */
  def this(v: Either[Either[Either[Option[Double], Rational], BigInt], Int]) = this(v, Scalar)

  /**
   * Method to determine if this is a valid Number.
   * An invalid number is of the has a value of form Left(Left(Left(None)))
   *
   * @return true if this is a valid Number
   */
  def isValid: Boolean = maybeDouble.isDefined

  /**
   * Method to get the value of this Number as a Double.
   *
   * @return an Option of Double.
   */

  def toDouble: Option[Double] = normalize.maybeDouble

  /**
   * Method to get the value of this Number as a Rational.
   * If this is actually a Double, it will be converted to a Rational according to the implicit conversion from Double to Rational.
   * See Rational.doubleToRational(x).
   *
   * @return an Option of Rational.
   */
  def toRational: Option[Rational] = normalize.maybeRational

  /**
   * Method to get the value of this Number as a BigInt.
   *
   * @return an Option of BigInt. If this Number cannot be converted to a BigInt, then None will be returned.
   */
  def toBigInt: Option[BigInt] = normalize.maybeBigInt

  /**
   * Method to get the value of this Number as an Int.
   *
   * @return an Option of Int. If this Number cannot be converted to an Int, then None will be returned.
   */
  def toInt: Option[Int] = normalize.maybeInt

  /**
   * Add x to this Number and return the result.
   * See Number.plus for more detail.
   *
   * @param x the addend.
   * @return the sum.
   */
  def +(x: Number): Number = Number.plus(this, x)

  /**
   * Subtract x from this Number and return the result.
   * See + and unary_ for more detail.
   *
   * @param x the subtrahend.
   * @return the difference.
   */
  def -(x: Number): Number = this + -x

  /**
   * Change the sign of this Number.
   */
  lazy val unary_- : Number = Number.negate(this)

  /**
   * Multiply this Number by x and return the result.
   * See Number.times for more detail.
   *
   * * @param x the multiplicand.
   * * @return the product.
   */
  def *(x: Number): Number = Number.times(this, x)

  /**
   * Divide this Number by x and return the result.
   * See * and invert for more detail.
   *
   * @param x the divisor.
   * @return the quotient.
   */
  def /(x: Number): Number = this * x.invert

  /**
   * Yields the inverse of this Number.
   * This Number is first normalized so that its factor is Scalar, since we cannot directly invert Numbers with other
   * factors.
   */
  lazy val invert: Number = Number.inverse(normalize)

  /**
   * Method to determine the sense of this number: negative, zero, or positive.
   *
   * @return an Int which is negative, zero, or positive according to the magnitude of this.
   */
  def signum: Int = Number.signum(this)

  /**
   * Render this Number in String form, including the factor.
   *
   * @return
   */
  override def toString: String = {
    val sb = new StringBuilder()
    sb.append(Number.optionMap(value)(_.toString, x => Number.optionMap(x)(_.toString, y => Number.optionMap(y)(_.toString, {
      case Some(n) => Some(n.toString)
      case None => None
    }))).getOrElse("<undefined>"))
    sb.append(factor.toString)
    sb.toString
  }

  // Following are methods which should be private

  /**
   * Method to "normalize" a number, that's to say make it a Scalar.
   *
   * @return a new Number with factor of Scalar but with the same magnitude as this.
   */
  def normalize: Number = factor match {
    case Scalar => this
    case Pi | E => (maybeDouble map (x => Number(x * factor.value))).getOrElse(Number())
  }

  /**
   * Method to create a new version of this, but with factor f.
   * NOTE: the result will have the same absolute magnitude as this.
   *
   * @param f the new factor for the result.
   * @return a Number based on this and factor.
   */
  def scale(f: Factor): Number = factor match {
    case `f` => this
    case _ => Number(maybeDouble map (_ * factor.value / f.value), f)
  }

  /**
   * Return a Number which uses the most restricted type possible.
   * A Number based on a Double will yield a Number based on a BigInt (if there is no fractional part).
   * A Number based on a Rational will yield a Number based on a BigInt (if there is a unit denominator).
   * A Number based on a BigInt will yield a Number based on a Int (if it is sufficiently small).
   *
   * @return a Number with the same magnitude as this.
   */
  lazy val specialize: Number = value match {
    // Int case
    case Right(_) => this
    // BigInt case
    case Left(Right(b)) =>
      val intValue = b.intValue
      if (BigInt(intValue) == b) Number(intValue, factor) else this
    // Rational case
    case Left(Left(Right(r))) =>
      Try(r.toBigInt) match {
        case Success(b) => Number(b, factor).specialize
        case _ => this
      }
    // Double case
    case Left(Left(Left(Some(x)))) =>
      // NOTE we do not convert Double to Rational because any Double
      // can be approximated by a Double to within some arbitrary tolerance.
      // If we can, without loss of information, we upgrade Double to BigInt then normalize the result.
      val y: Long = Math.round(x)
      if (y.toDouble == x) Number(y, factor).specialize else this
    // Invalid case
    case _ => this
  }

  /**
   * Method to align the factors of this and x such that the resulting Numbers (in the tuple) each have the same factor.
   *
   * @param x the Number to be aligned with this.
   * @return a tuple of two Numbers with the same factor.
   */
  def alignFactors(x: Number): (Number, Number) = factor match {
    case Scalar => (this, x.scale(factor))
    case _ => (scale(x.factor), x)
  }

  /**
   * Method to align the types of this and x such that the resulting Numbers (in the tuple) each have the same structure.
   *
   * @param x the Number to be aligned with this.
   * @return a tuple of two Numbers, the first of which will be the more general type:
   *         (Invalid vs. Double, Double vs. Rational, Rational vs. BigInt, BigInt vs. Int).
   */
  def alignTypes(x: Number): (Number, Number) = value match {
    // this is an invalid Number: return a pair of invalid numbers
    case Left(Left(Left(None))) => (this, this)
    // this value is a real Number: convert x to a Number based on real.
    case Left(Left(Left(Some(_)))) => x.value match {
      // x's value is invalid: swap the order so the the first element is invalid
      case Left(Left(Left(None))) => x.alignTypes(this)
      // otherwise: return this and x re-cast as a Double
      case _ => (this, x.maybeDouble.map(y => Number(y, x.factor)).getOrElse(Number()))
    }
    // this value is a Rational:
    case Left(Left(Right(_))) => x.value match {
      // x's value is a real Number: swap the order so that the first element is the real number
      case Left(Left(Left(_))) => x.alignTypes(this)
      // otherwise: return this and x re-cast as a Rational
      case _ => (this, Number(x.maybeRational.getOrElse(Rational.NaN), x.factor))
    }
    // this value is a BigInt:
    case Left(Right(_)) => x.value match {
      // x's value is a Rational or real Number: swap the order so that the first element is the Rational/real number
      case Left(Left(_)) => x.alignTypes(this)
      // otherwise: return this and x re-cast as a BigInt
      case _ => (this, Number(x.maybeBigInt.getOrElse(BigInt(0)), x.factor)) // FIXME Need to fix this
    }
    // this value is an Int:
    case Right(_) => x.value match {
      // x's value is a BigInt, Rational or real Number: swap the order so that the first element is the BigInt/Rational/real number
      case Left(_) => x.alignTypes(this)
      // otherwise: return this and x re-cast as an Int
      case _ => (this, Number(x.maybeInt.getOrElse(0), x.factor)) // FIXME Need to fix this
    }
  }

  /**
   * Evaluate a dyadic operator on this and other, using the various functions passed in.
   * NOTE: this and other must have been aligned by type so that they have the same structure.
   *
   * @param other     the other operand, a Number.
   * @param f         the factor to apply to the result.
   * @param fInt      the function to combine two Ints.
   * @param fBigInt   the function to combine two BigInts.
   * @param fRational the function to combine two Rationals.
   * @param fDouble   the function to combine two Doubles.
   * @return a new Number which is result of applying the appropriate function to the operands this and other.
   */
  private def composeDyadic(other: Number, f: Factor)(fInt: (Int, Int) => Int, fBigInt: (BigInt, BigInt) => BigInt, fRational: (Rational, Rational) => Rational, fDouble: (Double, Double) => Double): Option[Number] = {
    val xToZy0: Option[Double] => Try[Number] = {
      case Some(n) => Try(Number(fDouble(n, other.maybeDouble.get), f))
      case None => Failure(new NoSuchElementException())
    }
    val xToZy1: Either[Option[Double], Rational] => Try[Number] = y => Number.tryMap(y)(x => Number(fRational(x, other.maybeRational.get), f), xToZy0)
    val xToZy2: Either[Either[Option[Double], Rational], BigInt] => Try[Number] = x => Number.tryMap(x)(x => Number(fBigInt(x, other.toBigInt.get), f), xToZy1)
    Number.tryMap(value)(x => Number(fInt(x, other.maybeInt.get), f), xToZy2).toOption
  }

  /**
   * Evaluate a monadic operator on this, using the various functions passed in.
   *
   * @param fInt      the function to combine transform an Int.
   * @param fBigInt   the function to combine transform a BigInt.
   * @param fRational the function to combine transform a Rational.
   * @param fDouble   the function to combine transform a Double.
   * @return a new Number which is result of applying the appropriate function to the operand this.
   */
  private def composeMonadic(fInt: Int => Int, fBigInt: BigInt => BigInt, fRational: Rational => Rational, fDouble: Double => Double): Option[Number] = {
    val xToZy0: Option[Double] => Try[Number] = {
      case Some(n) => Try(Number(fDouble(n)))
      case None => Failure(new NoSuchElementException())
    }
    val xToZy1: Either[Option[Double], Rational] => Try[Number] = y => Number.tryMap(y)(x => Number(fRational(x), factor), xToZy0)
    val xToZy2: Either[Either[Option[Double], Rational], BigInt] => Try[Number] = x => Number.tryMap(x)(x => Number(fBigInt(x), factor), xToZy1)
    Number.tryMap(value)(x => Number(fInt(x), factor), xToZy2).toOption
  }

  /**
   * An optional Rational that corresponds to the value of this Number (but ignoring the factor).
   */
  private lazy val maybeRational: Option[Rational] = {
    val result: Try[Rational] = Number.tryMap(value)(Rational(_), x => Number.tryMap(x)(Rational(_), y => Number.tryMap(y)(x => x, {
      case Some(n) => Success(Rational.doubleToRational(n))
      case None => Failure(new NoSuchElementException())
    })))
    result.toOption
  }

  /**
   * An optional Double that corresponds to the value of this Number (but ignoring the factor).
   */
  private lazy val maybeDouble: Option[Double] = Number.optionMap(value)(_.toDouble, x => Number.optionMap(x)(_.toDouble, y => Number.optionMap(y)(_.toDouble, identity)))

  /**
   * An optional BigInt that corresponds to the value of this Number (but ignoring the factor).
   */
  private lazy val maybeBigInt: Option[BigInt] = {
    val xToZy0: Option[Double] => Try[BigInt] = {
      case Some(n) if Math.round(n) == n => Try(BigInt(n.toInt))
      case Some(n) => Failure(NumberException(s"toBigInt: $n is not integral"))
      case None => Failure(new NoSuchElementException())
    }
    val xToZy1: Either[Option[Double], Rational] => Try[BigInt] = y => Number.tryMap(y)(x => x.toBigInt, xToZy0)
    val xToZy2: Either[Either[Option[Double], Rational], BigInt] => Try[BigInt] = x => Number.tryMap(x)(identity, xToZy1)
    Number.tryMap(value)(BigInt(_), xToZy2).toOption
  }

  /**
   * An optional Int that corresponds to the value of this Number (but ignoring the factor).
   */
  private lazy val maybeInt: Option[Int] = {
    val xToZy0: Option[Double] => Try[Int] = {
      case Some(n) if Math.round(n) == n => Try(n.toInt)
      case Some(n) => Failure(NumberException(s"toInt: $n is not integral"))
      case None => Failure(new NoSuchElementException())
    }
    val xToZy1: Either[Option[Double], Rational] => Try[Int] = y => Number.tryMap(y)(x => x.toInt, xToZy0)
    val xToZy2: Either[Either[Option[Double], Rational], BigInt] => Try[Int] = x => Number.tryMap(x)(_.toInt, xToZy1)
    Number.tryMap(value)(identity, xToZy2).toOption
  }
}

object Number {

  /**
   * Method to construct a Number from an Int.
   *
   * @param x      the Int value.
   * @param factor the appropriate factor
   * @return a Number based on x.
   */
  def apply(x: Int, factor: Factor): Number = Number(Right(x), factor)

  /**
   * Method to construct a Number from a Long.
   *
   * @param x      the Long value.
   * @param factor the appropriate factor
   * @return a Number based on the value of x converted to BigInt.
   */
  def apply(x: Long, factor: Factor): Number = Number(BigInt(x), factor)

  /**
   * Method to construct a Number from a BigInt.
   *
   * @param x      the BigInt value.
   * @param factor the appropriate factor
   * @return a Number based on x.
   */
  def apply(x: BigInt, factor: Factor): Number = Number(Left(Right(x)), factor)

  /**
   * Method to construct a Number from a Rational.
   *
   * @param x      the BigInt value.
   * @param factor the appropriate factor
   * @return a Number based on x.
   */
  def apply(x: Rational, factor: Factor): Number = Number(Left(Left(Right(x))), factor)

  /**
   * Method to construct a Number from a BigDecimal.
   *
   * @param x      the BigDecimal value.
   * @param factor the appropriate factor
   * @return a Number based on x.
   */
  def apply(x: BigDecimal, factor: Factor): Number = {
    val r = Rational(x)
    Number(r, factor)
  }

  /**
   * Method to construct a Number from an optional Double.
   *
   * @param xo     an optional Double.
   * @param factor the appropriate factor
   * @return a Number based on xo.
   */
  def apply(xo: Option[Double], factor: Factor): Number = Number(Left(Left(Left(xo))), factor)

  /**
   * Method to construct a Number from a Double.
   *
   * @param x      the Double value.
   * @param factor the appropriate factor
   * @return a Number based on x.
   */
  def apply(x: Double, factor: Factor): Number = if (x == Double.NaN) apply(factor) else apply(Some(x), factor)

  /**
   * Method to construct an invalid Number.
   *
   * @param factor the appropriate factor
   * @return a invalid Number.
   */
  def apply(factor: Factor): Number = apply(None, factor)

  /**
   * Method to construct a Number from an Int.
   *
   * @param x the Int value.
   * @return a Number based on x.
   */
  def apply(x: Int): Number = Number(x, Scalar)

  /**
   * Method to construct a Number from a Long.
   *
   * @param x the Long value.
   * @return a Number based on the value of x converted to BigInt.
   */
  def apply(x: Long): Number = Number(x, Scalar)

  /**
   * Method to construct a Number from a BigInt.
   *
   * @param x the BigInt value.
   * @return a Number based on x.
   */
  def apply(x: BigInt): Number = Number(x, Scalar)

  /**
   * Method to construct a Number from a Rational.
   *
   * @param x the BigInt value.
   * @return a Number based on x.
   */
  def apply(x: Rational): Number = Number(x, Scalar)

  /**
   * Method to construct a Number from a BigDecimal.
   *
   * @param x the BigDecimal value.
   * @return a Number based on x.
   */
  def apply(x: BigDecimal): Number = {
    val r = Rational(x)
    Number(r, Scalar)
  }

  /**
   * Method to construct a Number from an optional Double.
   *
   * @param xo an optional Double.
   * @return a Number based on xo.
   */
  def apply(xo: Option[Double]): Number = Number(xo, Scalar)

  /**
   * Method to construct a Number from a Double.
   *
   * @param x the Double value.
   * @return a Number based on x.
   */
  def apply(x: Double): Number = apply(x, Scalar)

  /**
   * Method to construct an invalid Number.
   *
   * @return a invalid Number.
   */
  def apply(): Number = apply(Scalar)

  val numberParser = new NumberParser()

  /**
   * Method to parse a String and yield a Try[Number].
   *
   * @param w the String to be parsed.
   * @return a Number.
   */
  def parse(w: String): Try[Number] = {
    val ny: Try[Number] = numberParser.parse(numberParser.number, w) map (_.specialize)
    ny flatMap (n => if (n.isValid) Success(n) else Failure(NumberException(s"cannot parse $w as a Number")))
  }

  /**
   * Following are the definitions required by Ordering[Number]
   */
  trait NumberIsOrdering extends Ordering[Number] {
    def compare(x: Number, y: Number): Int = plus(x, negate(y)).signum
  }

  /**
   * Following are the definitions required by Numeric[Number]
   */
  trait NumberIsNumeric extends Numeric[Number] {
    /**
     * Adds two numbers and returns their sum.
     *
     * @param x the first number to be added
     * @param y the second number to be added
     * @return the sum of the two numbers
     */
    def plus(x: Number, y: Number): Number = Number.plus(x, y)

    /**
     * Subtracts the second number from the first */
    def minus(x: Number, y: Number): Number = Number.plus(x, negate(y))

    /**
     * Multiplies two numbers and returns the result.
     *
     * @param x the first number to be multiplied
     * @param y the second number to be multiplied
     * @return the product of the two numbers
     */
    def times(x: Number, y: Number): Number = Number.times(x, y)

    /**
     * Returns the negation of the given number.
     *
     * @ */
    def negate(x: Number): Number = Number.negate(x)

    /**
     * Converts an integer to a Number representation.
     *
     * @param x an integer value to be converted to a Number
     * */
    def fromInt(x: Int): Number = Number(x)

    /**
     * Attempts to parse the given string into an instance of a `Number`.
     *
     * @param str the string to be parsed
     */
    def parseString(str: String): Option[Number] = Number.parse(str).toOption

    /**
     * Converts the given Number to an Int.
     *
     * @param x the Number to be converted to an Int
     * @return the integer value of the given Number
     */
    def toInt(x: Number): Int = toLong(x).toInt

    /**
     * Converts the given `Number` instance to a `Long`.
     *
     * */
    def toLong(x: Number): Long = x.toBigInt match {
      case Some(y) => y.toLong
      case None => x.maybeRational match {
        case Some(r) => r.toLong
        case None => x.maybeDouble match {
          case Some(z) => Math.round(z)
          case None => throw NumberException("toLong: this is invalid")
        }
      }
    }

    /**
     * Converts a Number to a Double if possible.
     *
     * @param x the Number to convert
     * @return the Double value of the Number
     * @throws NumberException if the conversion is not valid
     */
    def toDouble(x: Number): Double = x.maybeDouble match {
      case Some(y) => y
      case None => throw NumberException("toDouble: this is invalid")
    }

    /**
     * Converts the given `Number` to a `Float`.
     *
     * @param x the `Number` instance to be converted
     */
    def toFloat(x: Number): Float = toDouble(x).toFloat
  }

  /**
   * Following are the definitions required by Fractional[Number]
   *
   * TODO fix this.
   */
  trait NumberIsFractional extends Fractional[Number] {
    /**
     * Divides one number by another.
     *
     * @param x the numerator
     * @param y the denominator
     * @return the result of dividing x by y
     */
    def div(x: Number, y: Number): Number =
      Number.times(x, inverse(y))
  }

  implicit object NumberIsFractional extends NumberIsFractional with NumberIsNumeric with NumberIsOrdering

  /**
   * Converts an `Option[X]` to an `Either[Y, X]`. If the given `Option` contains a value, it is wrapped in a `Right`.
   * Otherwise, the fallback value is evaluated and wrapped in a `Left`.
   *
   * @param x the `Option[X]` to convert.
   * @param y the fallback value of type `Y`, which will be used if `x` is `None`.
   * @return an `Either[Y, X]` containing `Right[X]` if `x` is `Some`, or `Left[Y]` if `x` is `None`.
   */
  //noinspection ScalaUnusedSymbol
  private def optionToEither[X, Y](x: Option[X], y: => Y): Either[Y, X] = x.map(Right(_)).getOrElse(Left(y))

  /**
   * Transforms an `Either[X, Y]` to a `Try[Z]` using provided transformation functions.
   *
   * @param xYe   the input `Either[X, Y]`; where `Left[X]` represents an error and `Right[Y]` represents success.
   * @param yToZ  a function that transforms the `Y` value in `Right[Y]` to a `Z` value.
   * @param xToZy a function that transforms the `X` value in `Left[X]` to a `Try[Z]`.
   * @return a `Try[Z]` representation of the input, containing `Success[Z]` if transformation was successful,
   *         or a `Failure` if input and transformations are invalid or fail.
   */
  private def tryMap[X, Y, Z](xYe: Either[X, Y])(yToZ: Y => Z, xToZy: X => Try[Z]): Try[Z] = xYe.toOption.map(yToZ) match {
    case Some(z) => Success(z)
    case None => xYe.left.toOption.map(xToZy) match {
      case Some(z) => z
      case None => Failure(new NoSuchElementException)
    }
  }

  /**
   * Transforms an `Either[X, Y]` to an `Option[Z]` using provided transformation functions.
   *
   * @param xYe   the input `Either[X, Y]`; where `Left[X]` represents an alternative and `Right[Y]` contains the primary value.
   * @param yToZ  a function that transforms the `Y` value in `Right[Y]` to a `Z` value.
   * @param xToZy a function that transforms the `X` value in `Left[X]` to an optional `Z` value.
   * @return an `Option[Z]`, containing `Some[Z]` if transformation of `Right[Y]` or `Left[X]` produces a result, or `None` otherwise.
   */
  private def optionMap[X, Y, Z](xYe: Either[X, Y])(yToZ: Y => Z, xToZy: X => Option[Z]): Option[Z] = xYe.toOption.map(yToZ) match {
    case Some(z) => Some(z)
    case None => xYe.left.toOption.flatMap(xToZy)
  }

  /**
   * Adds two Number instances and returns their sum.
   *
   * @param x the first Number to add
   * @param y the second Number to add
   * @return the resulting Number after adding x and y
   */
  private def plus(x: Number, y: Number): Number = {
    val (a, b) = x.alignFactors(y)
    val (p, q) = a.alignTypes(b)
    p.composeDyadic(q, p.factor)(_ + _, _ + _, _ + _, _ + _).getOrElse(Number()).specialize
  }

  /**
   * Multiplies two `Number` instances and returns the result as a `Number`.
   *
   * @param x the first `Number` operand.
   * @param y the second `Number` operand.
   * @return the product of `x` and `y` as a `Number`.
   */
  private def times(x: Number, y: Number): Number = {
    val (p, q) = x.alignTypes(y)
    val factor = p.factor + q.factor
    p.composeDyadic(q, factor)(_ * _, _ * _, _ * _, _ * _).getOrElse(Number()).specialize
  }

  /**
   * Negates the given Number, producing its additive inverse.
   *
   * @param x the Number to be negated
   * @return the negated Number, or a default Number if negation fails
   */
  private def negate(x: Number): Number = x.composeMonadic(-_, -_, _.negate, -_).getOrElse(Number())

  /**
   * Computes the multiplicative inverse of the given Number.
   *
   * If the input is 1 (or equivalent), it simply returns the same Number.
   * Otherwise, it attempts to compute the inverse based on the type of the value.
   * Special, invalid, or undefined cases may yield a specialized or default Number.
   *
   * @param x the Number for which the inverse is to be computed.
   * @return the multiplicative inverse of the input Number, or a specialized/invalid Number
   *         if the operation is undefined for the input.
   */
  private def inverse(x: Number): Number = {
    val maybeNumber = x.value match {
      // First we take care of the special cases
      case Right(1) => Some(x)
      case Left(Right(_)) if x.toBigInt.get == BigInt(1) => Some(x)
      case Left(Left(Left(_))) => x.maybeDouble.map(1 / _).map(Number(_))
      case _ => x.maybeRational.map(_.invert).map(Number(_))
    }
    maybeNumber.getOrElse(Number()).specialize
  }

  private def signum(x: Number): Int =
    x.toInt.map(_.signum)
            .orElse(x.toBigInt.map(_.signum))
            .orElse(x.toRational.map(_.signum))
            .orElse(x.toDouble.map(d => Math.signum(d).toInt))
            .getOrElse(throw new NoSuchElementException("signum: invalid Number"))
}

/**
 * A trait representing a mathematical factor with a double value.
 *
 * This trait defines a `value` property to retrieve the numeric value of the factor
 * and a `+` operator to allow adding two factors together.
 */
sealed trait Factor {
  /**
   * Retrieves the numeric value of the factor.
   *
   * @return the double value representing the factor
   */
  def value: Double

  /**
   * Adds this `Factor` to another `Factor` and returns the resulting `Factor`.
   *
   * @param other the other `Factor` to be added to this `Factor`
   * @return the resulting `Factor` representing the sum of this `Factor` and the `other` `Factor`
   */
  def +(other: Factor): Factor
}

/**
 * A sealed abstract class representing a mathematical non-scalar factor, which extends the `Factor` trait.
 *
 * The `NonScalarFactor` class provides behavior for addition involving other factors and enforces
 * the restriction that non-scalar factors cannot be added together.
 *
 * Functionality includes:
 * - Defining a custom implementation for the `+` operator.
 */
sealed abstract class NonScalarFactor extends Factor {
  /**
   * Adds this `Factor` to another `Factor` and returns the resulting `Factor`.
   *
   * If the other `Factor` is `Scalar`, this method will return the current `Factor`.
   * If the other `Factor` is non-`Scalar`, it throws a `NumberException` since
   * non-`Scalar` factors cannot be added together.
   *
   * @param other the other `Factor` to be added to this `Factor`
   * @return the resulting `Factor` representing the sum of this `Factor` and the `other` `Factor`
   * @throws NumberException if the other `Factor` is non-`Scalar`
   */
  def +(other: Factor): Factor = other match {
    case Scalar =>
      this
    case _ =>
      throw NumberException("cannot add non-Scalar factors together")
  }
}

/**
 * An object representing the scalar value of a mathematical factor.
 *
 * Scalar is a special case of a `Factor` with a constant value of 1. It is
 * effectively the identity element in the context of multiplying or adding
 * factors. This object provides a specific implementation of the `Factor` trait
 * methods.
 */
case object Scalar extends Factor {
  /**
   * Retrieves the scalar value of this factor.
   *
   * @return the constant scalar value `1.0`
   */
  def value: Double = 1

  override def toString: String = ""

  /**
   * Adds this `Factor` to another `Factor` and returns the resulting `Factor`.
   *
   * @param other the other `Factor` to add to this `Factor`
   * @return the resulting `Factor` after adding this `Factor` and the `other` `Factor`
   */
  def +(other: Factor): Factor = other
}

case object Pi extends NonScalarFactor {
  /**
   * Computes and returns the mathematical constant Pi (π).
   *
   * @return the value of Pi as a Double
   */
  def value: Double = Math.PI

  override def toString: String = Factor.sPi
}

/**
 * An object representing the mathematical constant e (Euler's number).
 *
 * This case object is a specific implementation of the `Factor` trait, providing
 * the value of e and handling addition operations with other factors.
 */
case object E extends Factor {
  /**
   * Retrieves the mathematical constant e (Euler's number).
   *
   * @return the constant value of e (approximately 2.718281828459045)
   */
  def value: Double = Math.E

  override def toString: String = "e"

  /**
   * Adds this `Factor` to another `Factor` and returns the resultant `Factor`.
   *
   * This method specifically handles the addition of `Scalar` to `Factor`.
   * If the `other` factor is `Scalar`, it returns the current instance.
   * Otherwise, it throws a `NumberException` as only `Scalar` addition is supported.
   *
   * @param other the `Factor` to be added to this `Factor`
   * @return the resulting `Factor` if the `other` is `Scalar`, otherwise this method throws a `NumberException`
   * @throws NumberException if the `other` `Factor` is not a `Scalar`
   */
  def +(other: Factor): Factor = other match {
    case Scalar =>
      this
    case _ =>
      throw NumberException("cannot add non-Scalar factors together")
  }
}

/**
 * Companion object for the `Factor` trait.
 *
 * This object provides utility methods and constants for creating specific instances of `Factor`.
 * It also contains predefined constants such as mathematical symbols and their alternatives for better flexibility.
 */
object Factor {
  val sE = "\uD835\uDF00"
  val sPi = "\uD835\uDED1"
  val sPiAlt0 = "pi"
  val sPiAlt1 = "Pi"
  val sPiAlt2 = "PI"

  /**
   * Returns a `Factor` based on the provided string representation.
   *
   * The method maps specific string inputs to predefined mathematical constants
   * (`Pi` or `E`) or returns `Scalar` as a default value if no match is found.
   *
   * @param w the string representation of the factor, such as "pi" or "e"
   * @return a `Factor` instance corresponding to the input string,
   *         where "pi" and its variations return `Pi`, "e" returns `E`,
   *         and all other inputs return `Scalar`
   */
  def apply(w: String): Factor = w match {
    case `sPi` | `sPiAlt0` | `sPiAlt1` | `sPiAlt2` => Pi
    case `sE` => E
    case _ => Scalar
  }
}

/**
 * A custom exception to represent errors related to numerical operations.
 *
 * @param str the detailed message explaining the nature of the exception
 */
case class NumberException(str: String) extends Exception(str)
