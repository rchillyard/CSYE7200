package edu.neu.coe.csye7200.lab99.exams

import edu.neu.coe.csye7200.lab99.exams.Rational.rootOfBigInt
import edu.neu.coe.csye7200.lab99.exams.TryOps.toTryWithRationalException
import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
 * Case class to represent a rational number.
 *
 * NOTE that `n` and `d` must be co-prime (no common factors), although we don't explicitly check that in this fragment.
 * For this reason, applications should use the `Rational.apply` method instead of the constructor.
 *
 * XXX A (4) What is the significance of the private keyword in the case class declaration?
 * XXX B (6) Why do we need it here?
 *
 * @param n the numerator.
 * @param d the denominator.
 */
case class Rational private (n: BigInt, d: BigInt) {

  def +(that: Rational): Rational = Rational.plus(this, that)

  def +(that: BigInt): Rational = this + Rational(that)

  def +(that: Long): Rational = this + Rational(that)

  def -(that: Rational): Rational = Rational.minus(this, that)

  def -(that: BigInt): Rational = this - Rational(that)

  def *(that: Rational): Rational = Rational.times(this, that)

  def *(that: BigInt): Rational = this * Rational(that)

  def *(that: Long): Rational = this * Rational(that)

  def *(that: Short): Rational = this * Rational(that)

  def /(that: Rational): Rational = this * that.invert

  def /(that: Long): Rational = this / Rational(that)

  def abs: Rational = if (signum < 0) negate else this

  def sqrt: Try[Rational] = power(Rational.half)

  def invert: Rational = Rational(d, n)

  def isWhole: Boolean = d == 1L

  def isUnity: Boolean = n == 1L && isWhole

  def toDouble: Double = Rational.toDouble(this)

  /**
   * Method to raise `this Rational` to a power `x` which is itself a `Rational`.
   *
   * XXX C (4) Under what circumstances would either of the first two exceptional conditions occur?
   * XXX D (5) Explain (briefly!) why a for-comprehension is used here.
   *
   * @param x the power by which `this` should be raised.
   * @return a `Rational` such that if we raise the result to the power of `x.invert`, we should get `this` back.
   */
  def power(x: Rational): Try[Rational] = for {
    p <- toTryWithRationalException(Rational.toInt(x.n), s"power($x): numerator is not an Int")
    r <- toTryWithRationalException(Rational.toInt(x.d), s"power($x): denominator is not an Int")
    z <- toTryWithRationalException(power(p).root(r), s"power($x): cannot calculate result exactly")
  } yield z

  /**
   * This method can be used to yield the value of `this` raised to the power of `x` when the `power` method returns a `Failure`.
   *
   * XXX E (6) define an appropriate return type for this method. You may have to think outside the box a little.
   * TODO F (5) implement the method (the logic should attempt to use power(Rational) first).
   *
   * @param x the power by which `this` should be raised.
   * @return ???
   */
  def powerAlt(x: Rational) = ???

  /**
   * Method to raise `this Rational` to a power `p` which is itself an `Int`.
   *
   * @param p an Int.
   * @return a Rational whose value is `this` multiplied by itself `p` times.
   */
  def power(p: Int): Rational = {

    /**
     * Tail-recursive method that multiplies `this` by `r` `x` times.
     *
     * TODO G (7) implement this tail-recursive method.
     *
     * @param r a `Rational` which represents the current result and which will be returned when the recursion terminates.
     * @param x the number of times that `r` should be multiplied by `this`.
     * @return `if x==0 then r` otherwise the result of invoking `inner` recursively with the appropriate parameters.
     */
    @tailrec def inner(r: Rational, x: Int): Rational = if (x == 0) r else inner(r * this, x - 1)

    // XXX H (4) is it really necessary to catch these special cases?
    if (p == 0) Rational.one
    else if (p == 1 || isUnity) this
    else {
      val result = inner(Rational.one, math.abs(p))
      if (p > 0) result
      else result.invert
    }
  }

  def root(x: Int): Option[Rational] =
    // XXX I (4) is it really necessary to catch these special cases?
    if (x == 1 || isUnity) Some(this)
    else if (x <= 0) None
    else for (a <- rootOfBigInt(n, x); b <- rootOfBigInt(d, x)) yield Rational(a, b)

  private def signum: Int = n.signum

  private def negate: Rational = Rational.negate(this)

  private def applySign(negative: Boolean): Rational = if (negative) negate else this

  /**
   * Method to render `this Rational` as a `String`.
   *
   * @return a `String` of form either `n` or `n/d`.
   */
  override def toString: String = this match {
    // XXX J (6) explain the expressions that are returned in each of the cases. Include the name of the construct.
    // XXX K (3) identify any other similar expressions--but that are not prefixed by "s"--in this module or in the unit tests.
    case Rational(top, Rational.bigOne) => s"$top"
    case Rational(top, bottom) => s"$top/$bottom"
  }
}

object Rational {
  /**
   * XXX L (6) Explain how an implicit class is invoked.
   *
   * @param sc a StringContext (this has to do with "string interpolators")
   */
  implicit class RationalHelper(val sc: StringContext) extends AnyVal {
    // XXX M (5) explain the effect of the asterisk (*) in the following declaration.
    def r(args: Any*): Rational = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val sb = new StringBuffer()
      while (strings.hasNext) {
        val s = strings.next()
        if (s.isEmpty)
          if (expressions.hasNext)
            sb.append(expressions.next())
          else
            throw RationalException("r: logic error: missing expression")
        else
          sb.append(s)
      }
      if (expressions.hasNext)
        throw RationalException(s"r: ignored: ${expressions.next()}")
      else
        Rational(sb.toString)
    }
  }

  private val bigZero: BigInt = BigInt(0)
  private[exams] val bigOne: BigInt = BigInt(1)
  private[exams] val bigTwo: BigInt = BigInt(2)
  private val bigNegOne: BigInt = BigInt(-1)

  val zero: Rational = Rational(0)
  val one: Rational = Rational(bigOne)
  val two: Rational = Rational(bigTwo)
  val half: Rational = two.invert
  val NaN = new Rational(0, 0)
  val negZero = new Rational(0, -1)

  def apply(n: BigInt, d: BigInt): Rational = normalize(n, d)

  def apply(n: BigInt, d: Long): Rational = apply(n, BigInt(d))

  def apply(n: BigInt): Rational = apply(n, bigOne)

  def apply(n: BigInt, negative: Boolean): Rational = apply(n).applySign(negative)

  def apply(n: Long): Rational = apply(BigInt(n))

  def apply(n: Int): Rational = apply(BigInt(n))

  // XXX N (5) There's a code smell here. Explain what it is and why it smells.
  def apply(w: String): Rational = parse(w).get

  private def parse(w: String): Try[Rational] = RationalParser.parse(w)

  implicit def convertLong(x: Long): Rational = Rational(x)

  implicit def convertBigInt(x: BigInt): Rational = Rational(x)

  def rootOfBigInt(b: BigInt, x: Int): Option[BigInt] =
    Try(BigInt(math.round(math.pow(b.toDouble, 1.0 / x)))) match {
      case Success(z) if z.pow(x) == b => Some(z)
      case _ => None
    }

  /**
   * Method to yield a `Rational` from the numerator `n` and denominator `d` by canceling any common factors.
   *
   * @param n the numerator.
   * @param d the denominator.
   * @return the rational number defined by `n` and `d`.
   */
  @tailrec private def normalize(n: BigInt, d: BigInt): Rational = (n, d) match {
    // NOTE: this corresponds to negative zero: leave as is.
    // XXX O (6) explain what the following pattern means with particular reference to the back-tick characters.
    case (`bigZero`, `bigNegOne`) => new Rational(n, d)
    // XXX P (5) identify the pattern type and explain the purpose of the following case.
    case _ if d < 0 => normalize(-n, -d)
    case _ =>
      val g = n.gcd(d)
      g.signum match {
        case 0 => NaN
        case _ => new Rational(n / g, d / g)
      }
  }

  private def minus(x: Rational, y: Rational): Rational = plus(x, negate(y))

  private def negate(x: Rational): Rational = Rational(-x.n, x.d)

  private def plus(x: Rational, y: Rational): Rational = (x, y) match {
    case (`negZero`, z) => z
    case (z, `negZero`) => z
    case _ => Rational((x.n * y.d) + (y.n * x.d), x.d * y.d)
  }

  private def times(x: Rational, y: Rational): Rational = (x, y) match {
    case (`negZero`, _) | (_, `negZero`) => zero
    case _ => Rational(x.n * y.n, x.d * y.d)
  }

  private def toDoubleViaString(x: BigInt) = x.toString().toDouble

  private def toDouble(x: Rational): Double =
    if (x eq negZero) -0.0
    else Try((BigDecimal(x.n) / BigDecimal(x.d)).toDouble).getOrElse(toDoubleViaString(x.n) / toDoubleViaString(x.d))

  def toInt(x: Rational): Try[Int] = if (x.isWhole && x.n.isValidInt) Success(x.n.toInt) else Failure(RationalException(s"$x is not whole"))

  private def toInt(x: BigInt): Option[Int] = if (x.isValidInt) Some(x.toInt) else None
}

object RationalParser {

  /**
   * Method to parse a `String` as a `Try[Rational]`.
   *
   * XXX Q (6) What is the `r` method: what type does it return?
   * XXX R (3) What is the name of the method that is invoked by the pattern in each case? Hint: it's not `unapply`.
   * XXX S (3) Why is it acceptable to use `null` in this method?
   * XXX T (3) [Bonus] You won't find it in `String`. Explain the mechanism by which the method `r` in `StringOps` is invoked.
   *
   * @param w the `String` representation of a rational number.
   * @return a `Try[Rational]`.
   */
  def parse(w: String): Try[Rational] = {
    val rRat = """^(\d+)(/(\d+))?$""".r
    w match {
      case rRat(n, _, null) => Success(Rational(n.toLong))
      case rRat(n, _, d) => Success(Rational(n.toLong, d.toLong))
      case rRat(n) => Success(Rational(n))
    }
  }
}

/**
 * XXX U (5) in the following methods, why is each of the default parameters declared with `=>`?
 */
object TryOps {
  /**
   * TODO V (4) implement this method to convert an `Option` into a `Try`.
   *
   * @param xo      an `Option[X]`.
   * @param default a `Try[X]`.
   * @tparam X the underlying type of both input and output.
   * @return if `xo` is `Some(x)` then `Success(x)` else `default`.
   */
  def toTry[X](xo: Option[X], default: => Try[X]): Try[X] = xo map (Success(_)) getOrElse default

  /**
   * TODO W (3) implement this method to convert an `Option` into a `Try` where the default is an exception, i.e., a `Throwable`.
   *
   * @param xo      an `Option[X]`.
   * @param default a `Throwable`.
   * @tparam X the underlying type of both input and output.
   * @return if `xo` is `Some(x)` then `Success(x)` else `Failure(default)`.
   */
  def toTryWithThrowable[X](xo: Option[X], default: => Throwable): Try[X] = xo.toRight(default).toTry // toTry(xo, Failure(default))

  /**
   * TODO X (3) implement this method to convert an `Option` into a `Try` where the default is a `String`.
   *
   * @param xo      an `Option[X]`.
   * @param default a `Throwable`.
   * @tparam X the underlying type of both input and output.
   * @return if `xo` is `Some(x)` then `Success(x)` else `Failure(RationalException(default))`.
   */
  def toTryWithRationalException[X](xo: Option[X], default: => String): Try[X] = toTryWithThrowable(xo, RationalException(default))

}

case class RationalException(s: String) extends Exception(s)