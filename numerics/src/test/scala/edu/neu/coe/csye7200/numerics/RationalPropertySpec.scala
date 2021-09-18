package edu.neu.coe.csye7200.numerics


import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

import scala.util.control.NonFatal

/**
  * Created by scalaprof on 10/4/16.
  */
class RationalPropertySpec extends Properties("Rational") {

  import Rational.RationalHelper

  property("FromString") = forAll { (a: Int, b: Short) =>
    val r = r"$a/$b"
    Rational.hasCorrectRatio(r, a, b)
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

  property("Double") = forAll { x: Double =>
    import org.scalactic.Tolerance._
    import org.scalactic.TripleEquals._
    // TODO check this is OK. Might need to be Rational(BigDecimal.valueOf(x))
    val r = Rational(x)
    val s = Rational(1.0 / x)
    (r * s).toDouble === 1.0 +- 1E-7
  }

}
