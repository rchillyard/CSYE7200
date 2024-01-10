package edu.neu.coe.csye7200.asstll

import edu.neu.coe.csye7200.asstll.FibonacciPythagoras.{fermatTree, platoTree, pythagorasTree, tree}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class FibonacciPythagorasSpec extends AnyFlatSpec with should.Matchers {

  behavior of "FibonacciPythagoras"

  it should "tree" in {
    tree take 13 to List shouldBe List(FibSquare(1, 2), FibSquare(1, 4), FibSquare(2, 5), FibSquare(2, 3), FibSquare(1, 6), FibSquare(4, 9), FibSquare(4, 7), FibSquare(2, 9), FibSquare(5, 12), FibSquare(5, 8), FibSquare(2, 7), FibSquare(3, 8), FibSquare(3, 4))
  }

  it should "fermatTree" in {
    fermatTree take 8 to List shouldBe List(FibSquare(1, 2), FibSquare(2, 5), FibSquare(5, 12), FibSquare(12, 29), FibSquare(29, 70), FibSquare(70, 169), FibSquare(169, 408), FibSquare(408, 985))
  }

  it should "square" in {

  }

  it should "pythagorasTree" in {
    pythagorasTree take 10 to List shouldBe List(FibSquare(1, 2), FibSquare(2, 3), FibSquare(3, 4), FibSquare(4, 5), FibSquare(5, 6), FibSquare(6, 7), FibSquare(7, 8), FibSquare(8, 9), FibSquare(9, 10), FibSquare(10, 11))
  }

  it should "root" in {

  }

  it should "platoTree" in {
    platoTree take 10 to List shouldBe List(FibSquare(1, 2), FibSquare(1, 4), FibSquare(1, 6), FibSquare(1, 8), FibSquare(1, 10), FibSquare(1, 12), FibSquare(1, 14), FibSquare(1, 16), FibSquare(1, 18), FibSquare(1, 20))
  }

}