package edu.neu.coe.csye7200.numerics

/**
  * LazyNumber is an abstract (base) class for Valuable objects.
  * The value of a LazyNumber is determined by applying the function f to the nominal value x.
  * A LazyNumber is lazy in two senses:
  * <ol>
  * <li>the nominal (initial) value of a LazyNumber is call-by-name so is not evaluated until needed.</li>
  * <li>the any function applied to the number, via map or flatMap, is composed with the existing function
  * such that the actual value is not evaluated until necessary.</li>
  * </ol>
  *
  * @author scalaprof
  */
abstract class LazyNumber[X: Fractional](x: => X, f: X => X) extends Valuable[X] with Fractional[LazyNumber[X]] {
  // The following println is for debugging purposes
  //  println(s"""LazyNumber: $x, f: $f; f(x)=${get}""")
  val z: Fractional[X] = implicitly[Fractional[X]]

  def get: X = f(x)

  // XXX Could we use CanBuildFrom/Builder here?
  def construct(x: => X, f: X => X): LazyNumber[X]

  def unit(x: => X): LazyNumber[X] = construct(x, identity)

  // 7 points
  def flatMap(g: X => LazyNumber[X]): LazyNumber[X] = ??? // TO BE IMPLEMENTED
  def map(f: X => X): LazyNumber[X] = flatMap(a => unit(f(a)))

  def fNegate: Product[X] = Product(z.negate(z.one))

  def fInvert: Named[X] = Named("invert", { x: X => z.div(z.one, x) })

  def fAdd(y: => LazyNumber[X]): Sum[X] = Sum(y.get)

  def fMult(y: => LazyNumber[X]): Product[X] = Product(y.get)

  def fDiv(y: => LazyNumber[X]): Product[X] = Product(z.div(z.one, y.get))

  // Operators for LazyNumber
  def +(that: LazyNumber[X]): LazyNumber[X] = plus(this, that)

  def -(that: LazyNumber[X]): LazyNumber[X] = minus(this, that)

  def unary_- : LazyNumber[X] = negate(this)

  def *(that: LazyNumber[X]): LazyNumber[X] = times(this, that)

  def unary_/ : LazyNumber[X] = invert(this)

  def /(that: LazyNumber[X]): LazyNumber[X] = div(this, that)

  // Methods for Numeric[LazyNumber]
  def minus(x: LazyNumber[X], y: LazyNumber[X]): LazyNumber[X] = x.plus(x, y map fNegate)

  def negate(x: LazyNumber[X]): LazyNumber[X] = x map fNegate

  def plus(x: LazyNumber[X], y: LazyNumber[X]): LazyNumber[X] = x map fAdd(y)

  def times(x: LazyNumber[X], y: LazyNumber[X]): LazyNumber[X] = x map fMult(y)

  def div(x: LazyNumber[X], y: LazyNumber[X]): LazyNumber[X] = x map fDiv(y)

  def invert(x: LazyNumber[X]): LazyNumber[X] = x map fNegate

  def toDouble(x: LazyNumber[X]): Double = z.toDouble(x.get)

  def toFloat(x: LazyNumber[X]): Float = z.toFloat(x.get)

  def toInt(x: LazyNumber[X]): Int = z.toInt(x.get)

  def toLong(x: LazyNumber[X]): Long = z.toLong(x.get)

  def compare(x: LazyNumber[X], y: LazyNumber[X]): Int = z.compare(x.get, y.get)
}

object LazyNumber {
  def apply[X: Numeric](x: X): LazyNumber[X] =
    x match {
      case r@Rational(_, _) => LazyRational(r, identity).asInstanceOf[LazyNumber[X]]
      case l: Long => LazyRational(l).asInstanceOf[LazyNumber[X]]
      case i: Int => LazyRational(i).asInstanceOf[LazyNumber[X]]
      case d => LazyDouble(implicitly[Numeric[X]].toDouble(d)).asInstanceOf[LazyNumber[X]]
    }

  implicit object RationalIsLazyNumber extends LazyRational(Rational.zero, identity)

  implicit object DoubleIsLazyNumber extends LazyDouble(Double.NaN, identity)

  implicit object FuzzyIsLazyNumber extends LazyFuzzy(Exact(0), identity)

}

case class LazyRational(x: Rational, f: Rational => Rational) extends LazyNumber[Rational](x, f) {
  def construct(x: => Rational, f: Rational => Rational): LazyNumber[Rational] = LazyRational(x, f)

  def fromInt(x: Int): LazyRational = LazyRational(x)

  def parseString(str: String): Option[LazyNumber[Rational]] = Rational.parse(str).map(LazyRational(_)).toOption
}

case class LazyDouble(x: Double, f: Double => Double) extends LazyNumber[Double](x, f) {
  def construct(x: => Double, f: Double => Double): LazyNumber[Double] = LazyDouble(x, f)

  def fromInt(x: Int): LazyDouble = LazyDouble(x)

  def parseString(str: String): Option[LazyNumber[Double]] = str.toDoubleOption.map(LazyDouble(_))
}

case class LazyFuzzy(x: Fuzzy, f: Fuzzy => Fuzzy) extends LazyNumber[Fuzzy](x, f) {

  def construct(x: => Fuzzy, f: Fuzzy => Fuzzy): LazyNumber[Fuzzy] = LazyFuzzy(x, f)

  def fromInt(x: Int): LazyFuzzy = LazyFuzzy(Exact(x), identity)

  def parseString(str: String): Option[LazyNumber[Fuzzy]] = Fuzzy.parse(str).map(LazyFuzzy(_)).toOption
}

object LazyRational {
  def apply(x: Rational): LazyRational = apply(x, identity)

  def apply(x: Long): LazyRational = apply(Rational(x))

  def apply(x: Int): LazyRational = apply(Rational(x))
}

object LazyDouble {
  def apply(x: Double): LazyDouble = apply(x, identity)
}

object LazyFuzzy {

  def apply(x: Fuzzy): LazyFuzzy = apply(x, identity)
}
