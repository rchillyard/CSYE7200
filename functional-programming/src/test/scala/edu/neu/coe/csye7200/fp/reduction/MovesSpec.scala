package edu.neu.coe.csye7200.fp.reduction

import org.junit.Assert.{assertFalse, assertTrue}
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MovesSpec extends AnyFlatSpec with Matchers with Futures with ScalaFutures {

  behavior of "distance"
  it should "be correct" in {
    Point(0, 0).distance(Point(5, 5)) shouldBe 10
    Point(0, 1).distance(Point(10, 5)) shouldBe 14
    Point(2, 3).distance(Point(5, 3)) shouldBe 3
    Point(3, 2).distance(Point(3, 5)) shouldBe 3
  }
  behavior of "Moves1"
  val start = Point(1, 1)
  it should "be true for 1,1->3,5" in {
    assertTrue(Moves1(3, 5).valid(start))
  }

  it should "be false for 1,1->2,2" in {
    assertFalse(Moves1(2, 2).valid(start))
  }

  it should "be true for 1,1->1,1" in {
    assertTrue(Moves1(1, 1).valid(start))
  }

  it should "be true for 9,5->12,8" in {
    assertFalse(Moves1(12, 8).valid(Point(9, 5)))
  }

  it should "be true for 1,1->99,100" in {
    assertTrue(Moves1(99, 100).valid(start))
  }

  // This test can never run with Moves1.
  ignore should "be true for 35,13->455955547,420098884" in {
    assertTrue(Moves1(455955547, 420098884).valid(start))
  }

  behavior of "Moves2"
  val start2 = Point(2, 2)
  it should "be true for 0 steps" in {
    assertTrue(Moves2(start).valid(start))
  }

  it should "be true for 1 step" in {
    assertTrue(Moves2(start).valid(Point(1, 2)))
    assertTrue(Moves2(start).valid(Point(2, 1)))
    assertTrue(Moves2(start2).valid(Point(2, 4)))
    assertTrue(Moves2(start2).valid(Point(4, 2)))
  }

  it should "be true for 2 steps" in {
    assertTrue(Moves2(start).valid(Point(1, 3)))
    assertTrue(Moves2(start).valid(Point(2, 3)))
    assertTrue(Moves2(start).valid(Point(3, 2)))
    assertTrue(Moves2(start).valid(Point(3, 1)))
  }

  it should "be true for 3 steps" in {
    assertTrue(Moves2(start).valid(Point(1, 4)))
    assertTrue(Moves2(start).valid(Point(2, 5)))
    assertTrue(Moves2(start).valid(Point(3, 5)))
    assertTrue(Moves2(start).valid(Point(4, 3)))
    assertTrue(Moves2(start).valid(Point(3, 4)))
    assertTrue(Moves2(start).valid(Point(5, 3)))
    assertTrue(Moves2(start).valid(Point(5, 2)))
    assertTrue(Moves2(start).valid(Point(4, 1)))
  }

  it should "be true for 4 steps (just four samples)" in {
    assertTrue(Moves2(start).valid(Point(4, 5)))
    assertTrue(Moves2(start).valid(Point(5, 4)))
    assertTrue(Moves2(start).valid(Point(1, 5)))
    assertTrue(Moves2(start).valid(Point(5, 1)))
  }

  it should "be false for unreachable squares" in {
    assertFalse(Moves2(start).valid(Point(2, 2)))
    assertFalse(Moves2(start).valid(Point(3, 3)))
    assertFalse(Moves2(start).valid(Point(2, 4)))
    assertFalse(Moves2(start).valid(Point(4, 2)))
    assertFalse(Moves2(start).valid(Point(4, 4)))
    assertFalse(Moves2(start).valid(Point(5, 5)))
    for (x <- 3 to 5; y <- 3 to 5)
      assertFalse(Moves2(start2).valid(Point(x, y)))
  }

  it should "be true for 9,5->12,8" in {
    assertFalse(Moves2(Point(9, 5)).valid(Point(12, 8)))
  }

  it should "be true for 1,1->99,100" in {
    assertTrue(Moves2(start).valid(Point(99, 100)))
  }

  it should "be false for 35,13->455955547,420098884" in {
    assertFalse(Moves2(Point(35, 13)).valid(Point(455955547, 420098884)))
  }

  behavior of "Moves3"
  it should "be true for 0 steps" in {
    assertTrue(Moves3(start).valid(start))
  }

  it should "be true for 1 step" in {
    assertTrue(Moves3(start).valid(Point(1, 2)))
    assertTrue(Moves3(start).valid(Point(2, 1)))
    assertTrue(Moves3(start2).valid(Point(2, 4)))
    assertTrue(Moves3(start2).valid(Point(4, 2)))
  }

  it should "be true for 2 steps" in {
    assertTrue(Moves3(start).valid(Point(1, 3)))
    assertTrue(Moves3(start).valid(Point(2, 3)))
    assertTrue(Moves3(start).valid(Point(3, 2)))
    assertTrue(Moves3(start).valid(Point(3, 1)))
  }

  it should "be true for 3 steps" in {
    assertTrue(Moves3(start).valid(Point(1, 4)))
    assertTrue(Moves3(start).valid(Point(2, 5)))
    assertTrue(Moves3(start).valid(Point(3, 5)))
    assertTrue(Moves3(start).valid(Point(4, 3)))
    assertTrue(Moves3(start).valid(Point(3, 4)))
    assertTrue(Moves3(start).valid(Point(5, 3)))
    assertTrue(Moves3(start).valid(Point(5, 2)))
    assertTrue(Moves3(start).valid(Point(4, 1)))
  }

  it should "be true for 4 steps (just four samples)" in {
    assertTrue(Moves3(start).valid(Point(4, 5)))
    assertTrue(Moves3(start).valid(Point(5, 4)))
    assertTrue(Moves3(start).valid(Point(1, 5)))
    assertTrue(Moves3(start).valid(Point(5, 1)))
  }

  it should "be false for unreachable squares" in {
    assertFalse(Moves3(start).valid(Point(2, 2)))
    assertFalse(Moves3(start).valid(Point(3, 3)))
    assertFalse(Moves3(start).valid(Point(2, 4)))
    assertFalse(Moves3(start).valid(Point(4, 2)))
    assertFalse(Moves3(start).valid(Point(4, 4)))
    assertFalse(Moves3(start).valid(Point(5, 5)))
    for (x <- 3 to 5; y <- 3 to 5)
      assertFalse(Moves3(start2).valid(Point(x, y)))
  }

  it should "be true for 9,5->12,8" in {
    assertFalse(Moves3(Point(9, 5)).valid(Point(12, 8)))
  }

  it should "be true for 1,1->99,100" in {
    assertTrue(Moves3(start).valid(Point(99, 100)))
  }

  it should "be false for 35,13->455955547,420098884" in {
    assertFalse(Moves3(Point(35, 13)).valid(Point(455955547, 420098884)))
  }

}
