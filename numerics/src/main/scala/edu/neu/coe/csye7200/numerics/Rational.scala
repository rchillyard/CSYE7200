package edu.neu.coe.csye7200.numerics

import edu.neu.coe.csye7200.numerics.Rational.toStringThreshold
import edu.neu.coe.csye7200.parse.{RationalParser, RationalParserException}

import java.lang.Math._
import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}


/**
  * Trait to define the behavior of a Ring.
  *
  * @tparam T the parametric type of the Ring.
  */
trait Ring[T] {
  /**
    * The additive, associative operator.
    *
    * @param that another value of T.
    * @return the sum of this and that.
    */
  def +(that: T): T

  /**
    * The additive identity.
    */
  val iPlus: T

  /**
    * The multiplicative, associative operator.
    *
    * @param that another value of T.
    * @return the product of this and that.
    */
  def *(that: T): T

  /**
    * The multiplicative identity.
    */
  val iTimes: T
}

/**
  * Rational.
  *
  * This case class represents Rational numbers by a BigInt numerator and a BigInt denominator.
  * The numerator (n) and the denominator (d) may never share a common factor: if you try to construct a Rational with "new" where there is
  * a common factor, then an exception will be thrown. However, all of the apply methods ensure valid Rational instances by factoring out any such common factors.
  * Similarly, the denominator may not be negative: again, the apply methods will take care of this situation.
  *
  * The domain of Rational includes values with 0 denominator and any numerator (either -ve or +ve infinity) as well as
  * the value with 0 numerator and denominator (NaN).
  *
  * @author scalaprof
  */
case class Rational(n: BigInt, d: BigInt) extends Ring[Rational] {

  // Pre-conditions

  // NOTE: ensure that the denominator is non-negative.
  require(d >= 0L, s"Rational denominator is negative: $d")

  // NOTE: ensure that the numerator and denominator are relatively prime.
  require(n == 0L && d == 0L || Rational.areRelativelyPrime(n, d), s"Rational($n,$d): arguments have common factor: ${Rational.gcd(n, d)}")

  // Concrete properties of Ring[Rational]

  def *(that: Rational): Rational = Rational.times(this, that)

  def +(that: Rational): Rational = Rational.plus(this, that)

  val iPlus: Rational = Rational.zero

  val iTimes: Rational = Rational.one

  // Operators

  def +(that: BigInt): Rational = this + Rational(that)

  def +(that: Long): Rational = this + Rational(that)

  def -(that: Rational): Rational = Rational.minus(this, that)

  def -(that: BigInt): Rational = this - Rational(that)

  lazy val unary_- : Rational = negate

  lazy val negate: Rational = Rational.negate(this)

  def *(that: BigInt): Rational = this * Rational(that)

  def *(that: Long): Rational = Rational.times(this, that)

  def *(that: Short): Rational = Rational.times(this, that.toLong)

  def /(that: Rational): Rational = this * that.invert

  def /(that: Long): Rational = this / Rational(that)

  def ^(that: Int): Rational = power(that)

  // Other methods appropriate to Rational
  lazy val signum: Int = n.signum

  lazy val invert: Rational = Rational(d, n)

  lazy val isWhole: Boolean = d == 1L

  lazy val isZero: Boolean = n == 0L

  lazy val isUnity: Boolean = n == 1L && isWhole

  lazy val isInfinity: Boolean = d == 0L

  lazy val isNaN: Boolean = isZero && isInfinity

  lazy val toInt: Int = Rational.toInt(this)

  lazy val toLong: Long = Rational.toLong(this)

  lazy val toBigInt: BigInt = Rational.toBigInt(this).get

  lazy val toFloat: Float = Rational.toFloat(this)

  lazy val toDouble: Double = Rational.toDouble(this)

  def power(x: Int): Rational = {
    @tailrec def inner(r: Rational, x: Int): Rational = if (x == 0) r else inner(r * this, x - 1)

    if (x == 0) Rational.one
    else {
      val rational = inner(Rational.one, math.abs(x))
      if (x > 0) rational
      else rational.invert
    }
  }

  lazy val toBigDecimal: BigDecimal = BigDecimal(n) / BigDecimal(d)

  def compare(other: Rational): Int = Rational.compare(this, other)

  lazy val toRationalString = s"$n/$d"

  lazy val isExactDouble: Boolean = toBigDecimal.isExactDouble // Only work with Scala 2.11 or above

  def applySign(negative: Boolean): Rational = if (negative) negate else this

  def applyExponent(exponent: Int): Rational = this * Rational.exponent(exponent)

  /**
    * Method to determine the Mediant of two rational numbers.
    * See Wikipedia: Mediant (Mathematics).
    * The result will always be intermediate in value to this and other.
    * The Mediant plays a role in the Farey Sequence (see Wikipedia) and thus can be used to approximate
    * an irrational number as a rational number.
    *
    * @param other the other number.
    * @return the mediant of this and other.
    */
  def mediant(other: Rational): Rational = if (signum >= 0 && other.signum >= 0 && !isInfinity && !other.isInfinity)
    Rational(n + other.n, d + other.d)
  else Rational.NaN

  override def toString: String = render(toStringThreshold)

  def render(implicit threshold: Long): String = {
    if (isNaN) "NaN"
    else if (isInfinity) (if (n > 0) "+ve" else "-ve") + " infinity"
    else if (isWhole) toBigInt.toString
    else if (d > threshold || isExactDouble) toDouble.toString
    else toRationalString
  }
}

object Rational {

  implicit val toStringThreshold: Long = 1000000L
  /**
    * Method to approximate a Double as a Rational, regardless of the value of the Double.
    * Application code should always call this method, or Rational.apply(Double).
    *
    * @param x       the value to approximate.
    * @param epsilon (implicit) the tolerance.
    * @return a Rational such that the difference between the result and x is less than epsilon.
    */
  def approximateAny(x: Double)(implicit epsilon: Tolerance): Rational =
    if (x == Double.NaN) NaN
    else if (x.isPosInfinity) infinity
    else if (x.isNegInfinity) infinity.negate
    else if (x > 0) approximatePositive(x)
    else approximatePositive(-x).negate

  /**
    * Method to take a Double in the range 0 thru 1 and approximate it by a Rational number
    * to within the given tolerance (epsilon).
    *
    * @param x       the value to approximate (should be between 0 and 1).
    * @param epsilon (implicit) the tolerance.
    * @return a Rational such that the difference between the result and x is less than epsilon.
    * @throws IllegalArgumentException if x is not between 0 and 1.
    */
  def approximate(x: Double)(implicit epsilon: Tolerance): Rational = {
    require(x >= 0 && x <= 1, "Call doubleToRational instead of approximate")

    @scala.annotation.tailrec
    def inner(r1: Rational, r2: Rational): Rational = {
      if (math.abs(r1.toDouble - x) < epsilon.x) r1
      else {
        val mediant: Rational = r1 mediant r2
        if (mediant.toDouble > x) inner(r1, mediant) else inner(mediant, r2)
      }
    }

    inner(Rational.zero, Rational.one)
  }

  implicit class RationalHelper(val sc: StringContext) extends AnyVal {
    def r(args: Any*): Rational = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val sb = new StringBuffer()
      while (strings.hasNext) {
        val s = strings.next
        if (s.isEmpty) {
          if (expressions.hasNext)
            sb.append(expressions.next)
          else
            throw RationalException("r: logic error: missing expression")
        }
        else
          sb.append(s)
      }
      if (expressions.hasNext)
        throw RationalException(s"r: ignored: ${expressions.next}")
      else
        Rational(sb.toString)
    }
  }

  val bigZero: BigInt = BigInt(0)
  val bigOne: BigInt = BigInt(1)
  val zero: Rational = Rational(0)
  lazy val infinity: Rational = zero.invert
  val one: Rational = Rational(1)
  val ten: Rational = Rational(10)
  val two: Rational = Rational(2)
  lazy val half: Rational = two.invert
  lazy val NaN = new Rational(0, 0)

  /**
    * Method to construct a Rational from two BigInt values.
    * NOTE: the this method ensures that the numerator and denominator are normalized.
    *
    * @param n the numerator.
    * @param d the denominator.
    * @return a Rational with the same ratio as n/d.
    */
  def apply(n: BigInt, d: BigInt): Rational = normalize(n, d)

  /**
    * Method to construct a Rational from a BigInt numerator and a Long denominator.
    * NOTE: the this method ensures that the numerator and denominator are normalized.
    *
    * @param n the numerator.
    * @param d the denominator.
    * @return a Rational with the same ratio as n/d.
    */
  def apply(n: BigInt, d: Long): Rational = apply(n, BigInt(d))

  /**
    * Method to construct a Rational from a BigInt.
    *
    * @param n the value.
    * @return a Rational with the same value as n.
    */
  def apply(n: BigInt): Rational = apply(n, bigOne)

  /**
    * Method to construct a Rational from a BigInt with sign defined by "negative".
    *
    * @param n        the value.
    * @param negative if true then the sign of the result will be flipped.
    * @return a Rational with the same value as n.
    */
  def apply(n: BigInt, negative: Boolean): Rational = apply(n).applySign(negative)

  /**
    * Method to construct a Rational from a Long.
    *
    * @param n the value.
    * @return a Rational with the same value as n.
    */
  def apply(n: Long): Rational = apply(BigInt(n))

  /**
    * Method to construct a Rational from a Long.
    *
    * @param n the value.
    * @return a Rational with the same value as n.
    */
  def apply(n: Int): Rational = apply(n.toLong)

  /**
    * Method to construct a Rational based on an irrational Double.
    * The tolerance (epsilon) is determined by the implicit value defined in Tolerance.
    * NOTE: if you want to specify the epsilon yourself, then invoke approximateAny directly.
    *
    * NOTE: this method is designed for true Doubles, not floating-point representations of decimal numbers.
    * Such decimal numbers should be converted to BigDecimal first using BigDecimal.valueOf(x).
    *
    * @param x the Double value.
    * @return a Rational which closely approximates x.
    */
  def apply(x: Double): Rational = approximateAny(x)

  /**
    * Method to construct a Rational based on a BigDecimal.
    *
    * @param x the BigDecimal to convert.
    * @return a Rational which is equal to x.
    * @throws RationalException if x cannot be represented as a Rational.
    */
  def apply(x: BigDecimal): Rational =
    if (x.scale >= 0) {
      val e = BigDecimal.apply(10).pow(x.scale)
      (for (n <- (x * e).toBigIntExact; d <- e.toBigIntExact) yield Rational(n, d)) match {
        case Some(r) => r
        case None => throw RationalException(s"Rational.apply(BigDecimal): cannot represent $x as a Rational")
      }
    }
    else x.toBigIntExact match {
      case Some(b) => Rational(b)
      case None => throw RationalException(s"cannot get value from BigDecimal $x")
    }

  /**
    * Method to construct a Rational based on a String.
    * NOTE: this method is NOT safe. It is much better to invoke parse(w).
    *
    * @param w the String value.
    * @return a Rational corresponding to the value given by w.
    * @throws RationalParserException if w is malformed.
    */
  def apply(w: String): Rational = parse(w).get

  /**
    * Method to construct a Try[Rational] based on a String.
    *
    * @param w the String value.
    * @return either Success(Rational) with value corresponding to the value given by w
    *         or Failure(RationalParserException) if w is malformed.
    */
  def parse(w: String): Try[Rational] = RationalParser.parse(w)

  /**
    * Tail-recursive method to find the greatest common divisor of two BigInts.
    * NOTE: This is Euclid's method.
    *
    * @param a the first BigInt.
    * @param b the second BigInt.
    * @return if (b==0) a else gcd(b, a % b)
    */
  @tailrec def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a else gcd(b, a % b)

  private def areRelativelyPrime(n: BigInt, d: BigInt) = gcd(n.abs, d) == 1

  /**
    * Method to yield a Rational exponent (in the sense of the a literal Double: for example 1.0Ex).
    *
    * @param x the power to which we should raise 10.
    * @return 10 raised to the power x, expressed as a Rational.
    */
  def exponent(x: Int): Rational = ten.power(x)

  /**
    * Implicit object which bestows all of the Ordering, Numeric, etc. functionality to a Rational.
    */
  implicit object RationalFractional extends RationalIsFractional

  /**
    * Implicit converter from Double to Rational.
    *
    * @param x the value.
    * @return a Rational equal to or approximately equal to x.
    */
  implicit def doubleToRational(x: Double): Rational = Rational(x)

  /**
    * Implicit converter from Long to Rational.
    *
    * @param x the value.
    * @return a Rational equal to x.
    */
  implicit def longToRational(x: Long): Rational = Rational(x)

  /**
    * Implicit converter from BigInt to Rational.
    *
    * @param x the value.
    * @return a Rational equal to x.
    */
  implicit def bigIntToRational(x: BigInt): Rational = Rational(x)

  /**
    * Implicit converter from Float to Rational.
    *
    * CONSIDER is this actually necessary?
    *
    * @param x the value.
    * @return a Rational equal to x.
    */
  implicit def floatToRational(x: Float): Rational = Rational(x.toDouble)

  /**
    * Trait defined to support the methods of Ordering[Rational].
    */
  trait RationalIsOrdering extends Ordering[Rational] {
    def compare(x: Rational, y: Rational): Int = x.compare(y)
  }

  /**
    * Trait defined to support the methods of Numeric[Rational].
    */
  trait RationalIsNumeric extends RationalIsOrdering with Numeric[Rational] {
    def plus(x: Rational, y: Rational): Rational = x + y

    def minus(x: Rational, y: Rational): Rational = x - y

    def times(x: Rational, y: Rational): Rational = x * y

    def negate(x: Rational): Rational = Rational(-x.n, x.d)

    def fromInt(x: Int): Rational = Rational(x)

    def parseString(str: String): Option[Rational] = Rational.parse(str).toOption

    def toInt(x: Rational): Int = x.toInt

    def toLong(x: Rational): Long = x.toLong

    def toFloat(x: Rational): Float = x.toFloat

    def toDouble(x: Rational): Double = x.toDouble
  }

  /**
    * Trait defined to support the methods of Fractional[Rational].
    */
  trait RationalIsFractional extends RationalIsNumeric with Fractional[Rational] {
    def div(x: Rational, y: Rational): Rational = Rational.div(x, y)
  }

  // CONSIDER making this private or moving back into RationalSpec
  def hasCorrectRatio(r: Rational, top: BigInt, bottom: BigInt): Boolean = {
    val _a = r * bottom
    val result = bottom == 0 || _a.isInfinity || (_a.isWhole && _a.toBigInt == top)
    if (!result) throw RationalException(s"incorrect ratio: r=${r.n}/${r.d}, top=$top, bottom=$bottom, _a=${_a}, gcd=${Rational.gcd(top, bottom)}")
    result
  }

  /**
    * Method to process the numerator and denominator to ensure that the denominator is never zero and never shares a common factor with the numerator.
    *
    * @param n the numerator
    * @param d the denominator
    * @return a Rational formed from n and d.
    */
  @scala.annotation.tailrec
  private def normalize(n: BigInt, d: BigInt): Rational =
    if (d < 0) normalize(-n, -d) else {
      val g = gcd(n.abs, d)
      g.signum match {
        case 0 => Rational.NaN
        case _ => new Rational(n / g, d / g)
      }
    }

  private def minus(x: Rational, y: Rational): Rational = plus(x, negate(y))

  private def negate(x: Rational): Rational = Rational(-x.n, x.d)

  private def plus(x: Rational, y: Rational): Rational = Rational((x.n * y.d) + (y.n * x.d), x.d * y.d)

  private def times(x: Rational, y: Rational): Rational = Rational(x.n * y.n, x.d * y.d)

  private def toDoubleViaString(x: BigInt) = x.toString().toDouble

  private def toDouble(x: Rational): Double = try {
    (BigDecimal(x.n) / BigDecimal(x.d)).toDouble
  } catch {
    case NonFatal(_) => toDoubleViaString(x.n) / toDoubleViaString(x.d)
  }

  private def toFloat(x: Rational): Float = toDouble(x).toFloat

  private def narrow(x: Rational, max: BigInt): Try[BigInt] = for (b <- toBigInt(x); z <- narrow(b, max)) yield z

  private def narrow(x: BigInt, max: BigInt): Try[BigInt] =
    if (x.abs <= max) Success(x)
    else Failure(RationalException("narrow: loss of precision"))

  private def toLong(x: Rational): Long = (narrow(x, Long.MaxValue) map (_.toLong)).get

  private def toInt(x: Rational): Int = (narrow(x, Int.MaxValue) map (_.toInt)).get

  // CONSIDER making this public
  private def toBigInt(x: Rational): Try[BigInt] = if (x.isWhole) Success(x.n) else Failure(RationalException(s"toBigInt: $x is " + (if (x.d == 0L)
    "infinite" else "not whole")))

  private def div(x: Rational, y: Rational): Rational = x / y

  private def compare(x: Rational, y: Rational): Int = minus(x, y).signum

  private def approximatePositive(x: Double)(implicit epsilon: Tolerance) =
    if (x > 0.1 && x < 10) approximateSmall(x)
    else {
      val e: Int = getExponent(x)
      val scale = pow(2, -e)
      approximateSmall(x * scale) * Rational.two.power(e)
    }

  private def approximateSmall(x: Double)(implicit epsilon: Tolerance) =
    if (x < 1) approximate(x)
    else {
      val y = floor(x).toInt
      approximate(x - y) + y
    }
}

/**
  * Exception class for Rationals.
  *
  * @param s the cause as a String.
  */
case class RationalException(s: String) extends Exception(s)

/**
  * Value class to define Tolerance.
  *
  * @param x the tolerance (epsilon) value.
  */
case class Tolerance(x: Double) extends AnyVal

/**
  * Companion object to Tolerance.
  */
object Tolerance {
  /**
    * Standard tolerance (epsilon) of 10 to the power of -15.
    */
  implicit val standardTolerance: Tolerance = Tolerance(1E-15)
}
