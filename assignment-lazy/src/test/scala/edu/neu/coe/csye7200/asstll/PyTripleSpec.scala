package edu.neu.coe.csye7200.asstll

import edu.neu.coe.csye7200.asstll.FibonacciPythagoras.root
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PyTripleSpec extends AnyFlatSpec with should.Matchers {

  val triple: PyTriple = root.triple

  behavior of "PyTriple"

  it should "x" in {
    triple.x shouldBe BigInt(3)
  }

  it should "y" in {
    triple.y shouldBe BigInt(4)
  }

  it should "toString" in {
    triple.render shouldBe "{3,4,5}"
  }

  it should "z" in {
    triple.z shouldBe BigInt(5)
  }

}