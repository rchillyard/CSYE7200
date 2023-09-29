package edu.neu.coe.csye7200.lab99.exams

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

/**
 * Tests expected values thanks to [[https://r-knott.surrey.ac.uk/Fibonacci/cfCALC.html]]
 */
class ConFracSpec extends AnyFlatSpec with should.Matchers {

  private val bigOne = BigInt(1)
  private val bigTwo = BigInt(2)
  private val bigThree = BigInt(3)
  private val goldenRatio = 1.618033988749895

  behavior of "ConFrac"

  it should "get 3 convergents for phi" in {
    val c = ConFrac(LazyList.continually(1))
    c.convergents.take(3).toList.map(c => c.asTuple) shouldBe List((bigOne, bigOne), (bigTwo, bigOne), (bigThree, bigTwo))
  }
  it should "get all convergents" in {
    val c = ConFrac(LazyList.continually(1))
    c.convergents.take(39).toList.map(c => c.asTuple).last shouldBe(BigInt(102334155), BigInt(63245986))
  }

  it should "evaluate 10" in {
    val c = ConFrac(LazyList.continually(1))
    c.evaluate(10) shouldBe 1.6181818181818182 +- 1E-10
  }
  it should "evaluate 39" in {
    val c = ConFrac(LazyList.continually(1))
    c.evaluate(39) shouldBe 1.6180339887498947 +- 1E-10
  }

  it should "evaluate 1E-4" in {
    val c = ConFrac(LazyList.continually(1))
    c.evaluate(1E-4) shouldBe goldenRatio +- 1E-4
  }
  it should "evaluate 1E-15" in {
    val c = ConFrac(LazyList.continually(1))
    c.evaluate(1E-15) shouldBe goldenRatio +- 1E-15
  }
}