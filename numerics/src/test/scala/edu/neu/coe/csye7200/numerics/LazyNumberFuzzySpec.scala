package edu.neu.coe.csye7200.numerics

import org.scalatest.{ FlatSpec, Matchers }

/**
 * @author scalaprof
 */
class LazyNumberFuzzySpec extends FlatSpec with Matchers {
  
  import Fuzzy._
  val fuzz1 = LazyFuzzy(1)
  val fuzz2 = LazyFuzzy(1,Product(2))
  // XXX why can't we say x*x here?
  def squ(x: Fuzzy): Fuzzy = x.times(x,x)
  val fuzzSquare = Named[Fuzzy]("square",squ)
  val fuzz4 = fuzz2 map fuzzSquare
  val fuzzy = Exact(1)
  val p = fuzzy*fuzzy
  
	"fuzz1" should "be 1" in {
		fuzz1.get shouldBe (Fuzzy.one)
	}
  
  it should "be -1 after negate" in {
    (-fuzz1).get shouldBe (Fuzzy.one * -1)
  }
  
  it should "be 0 after minus(1)" in {
    (fuzz1.-(fuzz1)).get shouldBe (Fuzzy.zero)
  }
  
  "fuzz2" should "be 2" in {
    fuzz2.get shouldBe (Fuzzy.one+Fuzzy.one)
  }
  
  it should "be 4 when multiplied by itself" in {
    (fuzz2 * fuzz2).get shouldBe (Exact(4))
  }

  it should "be 1 when divided by itself" in {
    (fuzz2 / fuzz2).get shouldBe (Fuzzy.one)
  }

  it should "be 3 when added to one" in {
    (fuzz2 + fuzz1).get shouldBe (Exact(3))
  }

  ignore should "be 6 when added to one and three" in {
//    (fuzz2 + fuzz1 + LazyFuzzy(Exact(3))).get shouldBe (Exact(6))
  }

  ignore should "be 3 when added to one by explicit function" in {
//    val lr = fuzz2 map Named("add Rat.1",{ x => x+Fuzzy.one })
//    lr.get shouldBe (Fuzzy.one*3)
  }
  
  "fuzzy for comprehension" should "give 4" in {
    val z = for (x <- fuzz2 ) yield fuzzSquare(x)
    z.get should be (Exact(4))
  }
  
//  "fuzzy composition" should "work" in {
//    val p = fuzz1.map(ExpDifferentiable[Fuzzy]())
//    println(s"p: $p")
//    println(s"p.get: ${p.get}")
//    p.get should be (2.718281828459045)
//  }
//  ignore should "work with fuzzy 1" in {
//    val f = LazyFuzzy(Bounded(1,1E-3))
//    val p = f.map(ExpDifferentiable[Fuzzy]())
//    println(s"p: $p")
//    println(s"p.get: ${p.get}")
//    p.get should be (2.718281828459045)
//  }

//  it should "give 8" in {
//    val z = for (x <- fuzz2; y <- fuzz4 ) yield x*y
//    z.get should be (Exact(8))
//  }

}
