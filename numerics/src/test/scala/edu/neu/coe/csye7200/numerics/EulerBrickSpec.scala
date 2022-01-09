package edu.neu.coe.csye7200.numerics

import edu.neu.coe.csye7200.numerics.EulerBrick._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.language.postfixOps

class EulerBrickSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Factored"

  it should "work" in {
    10 hasFactor 2 shouldBe true
    64L hasFactor 8 shouldBe true
    64L hasFactor 10L shouldBe false
  }

  behavior of "makeTriples"

  it should "flatMap(makeTriples)" in {
    LazyList.from(1).map(_.toLong).flatMap(makeTriples).take(10).toList shouldBe List((12, 13, 22), (12, 14, 22), (12, 15, 22), (12, 16, 22), (12, 17, 22), (12, 18, 22), (12, 19, 22), (12, 20, 22), (12, 21, 22), (12, 22, 23))
  }

  it should "flatMap(makeEulerTriples)" in {
    LazyList.from(1).map(_.toLong).flatMap(makeEulerTriples).take(2).toList shouldBe List((44, 117, 240), (240, 252, 275))
  }

  it should "makeTriples" in {
    makeTriples(22).toList shouldBe List((12, 13, 22), (12, 14, 22), (12, 15, 22), (12, 16, 22), (12, 17, 22), (12, 18, 22), (12, 19, 22), (12, 20, 22), (12, 21, 22))
    makeTriples(23).toList shouldBe List((12, 22, 23))
    makeTriples(24).toList shouldBe List((11, 12, 24), (12, 22, 24))
    makeTriples(25).toList shouldBe List((12, 22, 25))
  }

  it should "stepFactorOr" in {
    stepFactorOr(1, factored = false, 10) shouldBe 10L
    stepFactorOr(1, factored = true, 10) shouldBe 1L
    stepFactorOr(10, factored = false, 10) shouldBe 1L
    stepFactorOr(10, factored = true, 10) shouldBe 1L
  }

  it should "stepFactorAnd" in {
    stepFactorAnd(1, factored = false, 10) shouldBe 10L
    stepFactorAnd(1, factored = true, 10) shouldBe 10L
    stepFactorAnd(10, factored = false, 10) shouldBe 10L
    stepFactorAnd(10, factored = true, 10) shouldBe 1L
  }

  behavior of "EulerBrick"
  it should "work for (44, 117, 240)" in {
    EulerBrick(44, 117, 240).isValid shouldBe true
    EulerBrick(85, 132, 720).isValid shouldBe true
    EulerBrick(1, 2, 3).isValid shouldBe false
  }

  behavior of "EulerBrick Object"

  it should "makeList 1" in {
    val xs = LazyList.from(1).map(_.toLong).flatMap(makeEulerTriples)
    val ps = makeList(xs)
    (ps take 2 toList) shouldBe List(EulerBrick(44, 117, 240), EulerBrick(240, 252, 275))
  }

  it should "makeList 240" in {
    val ps = makeList(240)
    (ps take 1 toList) shouldBe List(EulerBrick(44, 117, 240))
  }

  it should "makeBricks" in {
    makeBricks(20) shouldBe List(EulerBrick(44, 117, 240), EulerBrick(240, 252, 275), EulerBrick(88, 234, 480), EulerBrick(480, 504, 550), EulerBrick(140, 480, 693), EulerBrick(85, 132, 720), EulerBrick(132, 351, 720), EulerBrick(160, 231, 792), EulerBrick(720, 756, 825), EulerBrick(176, 468, 960), EulerBrick(960, 1008, 1100), EulerBrick(1008, 1100, 1155), EulerBrick(220, 585, 1200), EulerBrick(1200, 1260, 1375), EulerBrick(280, 960, 1386), EulerBrick(170, 264, 1440), EulerBrick(264, 702, 1440), EulerBrick(320, 462, 1584), EulerBrick(187, 1020, 1584), EulerBrick(1440, 1512, 1650))
  }
}
