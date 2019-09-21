package edu.neu.coe.csye7200.numerics



import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

/**
  * Created by scalaprof on 10/4/16.
  */
class RationalPropertySpec extends Properties("String") {

  import Rational.RationalHelper

  property("RationalFromString") = forAll { (a: Int, b: Int) =>
    val rat = r"$a/$b"
    (rat*b).toInt == a
  }

}
