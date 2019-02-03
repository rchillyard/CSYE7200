package edu.neu.coe.csye7200.numerics

import org.scalatest.{ FlatSpec, Matchers }

/**
 * @author scalaprof
 */
class LazyFunctionSpec extends FlatSpec with Matchers {
  
  val one = LazyRational(1)
  val double = Product(Rational(2))
  val half = Product(Rational.one/2)
  val genericdouble = Named("double",{x: Rational => x*2})

//  "Identity" should "combine invisibly with anything" in {
//    Identity[Rational]().compose(genericdouble) shouldBe (genericdouble)
//    Identity[Rational]().compose(double) shouldBe (double)
//    Identity[Rational]().compose(half) shouldBe (half)
//  }
//    
//	ignore should "combine invisibly with itself" in {
//		Identity[Rational]().compose(Identity[Rational]()) shouldBe (Identity[Rational]())
//	}
//		  "merge" should "merge double and half into Some" in {
//	    LazyFunction.merge(double, half) should matchPattern { case Some(Identity()) => }
//	  }
	  
//	  it should "merge double and double into Some(Product)" in {
//	    LazyFunction.merge(double, double) should matchPattern { case Some(Product(_)) => }
//	  }
  
//  it should "merge double and increment into None" in {
//    LazyFunction.merge(double, Sum(Rational(1))) should matchPattern { case None => }
//  }
  
  "Compose" should "yield identity result when complementary functions composed" in {
	    val doubleHalf = double.compose(half)
	    doubleHalf.apply(Rational.one) shouldBe Rational.one
	  }
	  
	  ignore should "form Identity when complementary functions provided" in {
	    val doubleHalf = double.compose(half)
	    doubleHalf shouldBe Identity[Rational]()
	  }
  
  it should "form Composed when non-complementary functions provided (1)" in {
    val doubleHalf = genericdouble.compose(half)
    doubleHalf should matchPattern { case Composed(genericdouble,half) => }
    doubleHalf(Rational.one) should be (Rational.one) 
  }
  
  it should "form Composed when non-complementary functions provided (2)" in {
    val increment = Sum(Rational.one)
    val halveAndAddOne = increment.compose(half)
    println(halveAndAddOne)
    halveAndAddOne should matchPattern { case Composed(increment,half) => }
    halveAndAddOne(Rational.one) should be (Rational(3)/2) 
  }
  
//  "differentiable composition" should "work" in {
//    ComposedDifferentiable(ExpDifferentiable[Fuzzy](),Product[Fuzzy](2))
//  }
  
}
