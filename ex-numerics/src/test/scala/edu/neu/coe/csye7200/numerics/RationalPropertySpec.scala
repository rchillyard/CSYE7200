package edu.neu.coe.csye7200.numerics


import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties
import scala.util.control.NonFatal

/**
 * Created by scalaprof on 10/4/16.
 */
class RationalPropertySpec extends Properties("Rational") {

  import Rational.RationalHelper

  property("FromString") = forAll { (a: Int, b: Short) =>
    val r = r"$a/$b"
    Rational.hasCorrectRatio(r, a, b.toInt)
  }

  property("FromIntAndShort") = forAll { (a: Int, b: Short) =>
    val _a: BigInt = BigInt(a) * 1000
    val r = Rational(_a, b)
    Rational.hasCorrectRatio(r, _a, b.toLong)
  }

  property("Addition") = forAll { (a: Long, b: Short, c: Int, d: Short) =>
    val r1 = Rational(a, b)
    val r2 = Rational(c, d)
    val r = r1 + r2
    //      println(s"$a/$b, $c/$d => $r1 + $r2 = $r")
    try Rational.hasCorrectRatio(r, (BigInt(a) * d.toInt) + (BigInt(c) * b.toInt), b.toLong * d)
    catch {
      case NonFatal(x) => throw new Exception(s"a=$a, b=$b, c=$c, d=$d => $r1 + $r2 = $r (${r.n}/${r.d}) caused by ${x.getLocalizedMessage}")
    }
  }

  // NOTE the precondition is not decoration. This property says that x and 1/x
  // multiply back to 1, which cannot hold when 1/x is not a number that exists:
  // 1.0 / x overflows to Infinity for every |x| below about 5.56E-309, which is
  // to say for the subnormals and the smallest normals, and 1.0 / 0.0 is Infinity
  // too. ScalaCheck draws 100 doubles per run and reaches one of those only
  // occasionally, so this failed perhaps one run in ten -- seen first as
  // "Falsified after 83 passed tests, ARG_0: 1.303231388552603E-309".
  // Rational is not at fault; the property was simply asserting something untrue.
  property("Double") = forAll { (x: Double) =>
    import org.scalactic.Tolerance.*
    import org.scalactic.TripleEquals.*
    val reciprocal = 1.0 / x
    (!x.isNaN && x != 0.0 && !reciprocal.isInfinite) ==> {
      val r = Rational(x)
      val s = Rational(reciprocal)
      (r * s).toDouble === 1.0 +- 1E-7
    }
  }

}