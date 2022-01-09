package edu.neu.coe.csye7200.numerics

import edu.neu.coe.csye7200.numerics.Pythagorean.{coprime, isPythagorean, isValid, isSquare, makeList, makePairs, makePythagoreanPairs, square}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.language.postfixOps

class PythagoreanSpec extends AnyFlatSpec with should.Matchers {

  behavior of "makePairs"

  it should "flatMap(makePairs)" in {
    LazyList.from(1).map(_.toLong).flatMap(makePairs).take(10).toList shouldBe List((1, 2), (1, 3), (2, 3), (1, 4), (2, 4), (3, 4), (1, 5), (2, 5), (3, 5), (4, 5))
  }

  it should "makePairs" in {
    makePairs(2).toList shouldBe List(1 -> 2)
    makePairs(3).toList shouldBe List(1 -> 3, 2 -> 3)
    makePairs(4).toList shouldBe List(1 -> 4, 2 -> 4, 3 -> 4)
  }

  behavior of "Pythagorean"
  it should "apply" in {
    Pythagorean(1L -> 2L) shouldBe Pythagorean(1L, 2L)
  }

  it should "makeList 1" in {
    val xs = LazyList.from(1).map(_.toLong).flatMap(makePairs)
    val ps = makeList(xs)
    (ps take 3 toList) shouldBe List(Pythagorean(3, 4), Pythagorean(6, 8), Pythagorean(5, 12))
  }
  it should "makeList 2" in {
    val ps: LazyList[Pythagorean] = makeList(1) filter (p => p.x < 100 && p.y < 100)
    (ps take 18 toList) shouldBe List(Pythagorean(3, 4), Pythagorean(5, 12), Pythagorean(15, 8), Pythagorean(7, 24), Pythagorean(21, 20), Pythagorean(9, 40), Pythagorean(35, 12), Pythagorean(11, 60), Pythagorean(45, 28), Pythagorean(33, 56), Pythagorean(13, 84), Pythagorean(63, 16), Pythagorean(55, 48), Pythagorean(39, 80), Pythagorean(77, 36), Pythagorean(65, 72), Pythagorean(99, 20), Pythagorean(91, 60))
  }

  it should "makePythagoreanPair" in {
    Pythagorean.makePythagoreanPair(1, 2) shouldBe(3, 4)
    Pythagorean.makePythagoreanPair(1, 3) shouldBe(8, 6)
    Pythagorean.makePythagoreanPair(2, 3) shouldBe(5, 12)
  }

  it should "makePythagoreanPairs" in {
    (makePythagoreanPairs(2) take 2 toList) shouldBe List((3, 4))
    (makePythagoreanPairs(3) take 10 toList) shouldBe List((5, 12))
    (makePythagoreanPairs(4) take 10 toList) shouldBe List((15, 8), (7, 24))
  }

  it should "coprime" in {
    coprime((1, 2)) shouldBe true
    coprime((3, 2)) shouldBe true
    coprime((3, 4)) shouldBe true
    coprime((3, 6)) shouldBe false
  }

  it should "isSquare" in {
    isSquare(9L) shouldBe true
    isSquare(5L) shouldBe false
  }

  it should "square" in {
    square(1) shouldBe 1L
    square(3) shouldBe 9L
  }

  it should "isValid" in {
    isValid(1, 1) shouldBe false
    isValid(3, 4) shouldBe true
    isValid(5, 12) shouldBe true
  }
}
