package edu.neu.coe.csye7200.numerics

import org.scalatest.{ FlatSpec, Matchers }

/**
 * @author scalaprof
 */
class LazyNumberRationalSpec extends FlatSpec with Matchers {
  
  val rat1 = LazyRational(1)
  val rat2 = LazyRational(1,Product(2))
  val square = Named[Rational]("square",{x=>x*x})
  val rat4 = rat2 map square

	"rat1" should "be 1" in {
		rat1.get shouldBe (Rational.one)
	}
  
  it should "be -1 after negate" in {
    (-rat1).get shouldBe (-Rational.one)
  }
  
  it should "be 0 after minus(1)" in {
    (rat1.-(rat1)).get shouldBe (Rational.zero)
  }
  
  "rat2" should "be 2" in {
    rat2.get shouldBe (Rational.one+Rational.one)
  }
  
  it should "be 4 when multiplied by itself" in {
    (rat2 * rat2).get shouldBe (Rational(4))
  }

  it should "be 1 when divided by itself" in {
    (rat2 / rat2).get shouldBe (Rational.one)
  }

  it should "be 3 when added to one" in {
    (rat2 + rat1).get shouldBe (Rational(3))
  }

  it should "be 6 when added to one and three" in {
    (rat2 + rat1 + LazyRational(3)).get shouldBe (Rational(6))
  }

  it should "be 3 when added to one by explicit function" in {
    val lr = rat2 map Named("add Rat.1",{ x => x+Rational.one })
    lr.get shouldBe (Rational.one*3)
  }
  
  "for comprehension" should "give 4" in {
    val z = for (x <- rat2 ) yield square(x)
    z.get should be (Rational(4))
  }

  it should "give 8" in {
    val z = for (x <- rat2; y <- rat4 ) yield x*y
    z.get should be (Rational(8))
  }

//  it should "give NoFunction" in {
//    val z = for (x <- two; if(x==Rational(1)); y <- four ) yield x*y
//    z.f should be (NoFunction())
//  }

}
