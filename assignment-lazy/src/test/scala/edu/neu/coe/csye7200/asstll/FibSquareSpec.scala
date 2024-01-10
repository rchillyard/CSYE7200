package edu.neu.coe.csye7200.asstll

import edu.neu.coe.csye7200.asstll.FibonacciPythagoras._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class FibSquareSpec extends AnyFlatSpec with should.Matchers {

  val root: FibSquare = FibonacciPythagoras.root

  behavior of "FibSquare"

  it should "render" in {
    root.render shouldBe "[1,1,2,3]"
  }

  it should "platoList" in {
    root.platoList take 2 to List shouldBe List(FibSquare(1, 2), FibSquare(1, 4))
  }

  it should "pythagoras" in {
    root.pythagoras shouldBe FibSquare(2, 3)
  }

  it should "post" in {
    root.post shouldBe BigInt(3)
  }

  it should "children" in {
    root.children
  }

  it should "pythagorasList" in {
    root.pythagorasList take 2 to List shouldBe List(FibSquare(1, 2), FibSquare(2, 3))
  }

  it should "pre" in {
    root.pre shouldBe BigInt(1)
  }

  it should "pair" in {
    root.pair shouldBe RationalPair(Rational(1, 3), Rational(1, 2))
  }

  it should "triple" in {
    root.triple shouldBe PyTriple(BigInt(3), BigInt(4), BigInt(5))
  }

  it should "u" in {
    root.u shouldBe BigOne
  }

  it should "v" in {
    root.v shouldBe BigTwo
  }

  it should "fermatList" in {
    root.fermatList take 2 to List shouldBe List(FibSquare(1, 2), FibSquare(2, 5))
  }

  it should "plato" in {
    root.plato shouldBe FibSquare(1, 4)
  }

  it should "fermat" in {
    root.fermat shouldBe FibSquare(2, 5)
  }

  it should "child" in {
    root.child(0, 0) shouldBe root
    root.child(0, 1) shouldBe FibSquare(1, 4)
    root.child(1, 1) shouldBe FibSquare(2, 5)
    root.child(1, 0) shouldBe FibSquare(2, 3)
  }

}