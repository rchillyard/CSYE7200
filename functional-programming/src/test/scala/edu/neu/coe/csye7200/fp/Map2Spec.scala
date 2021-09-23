package edu.neu.coe.csye7200.fp

import edu.neu.coe.csye7200.Map2
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * @author scalaprof
  */
class Map2Spec extends AnyFlatSpec with Matchers {

  "map2right" should "return Left(bad) for Right(1), Left(bad)" in {
    val x: Either[String, Int] = Right(1)
    val y: Either[String, Int] = Left("bad")
    Map2.map2right(x, y) {
      _ + _
    } shouldBe Left("bad")
  }
  it should "return Left(bad) for Left(bad), Right(1)" in {
    val y: Either[String, Int] = Right(1)
    val x: Either[String, Int] = Left("bad")
    Map2.map2right(x, y) {
      _ + _
    } shouldBe Left("bad")
  }
  it should "return Right(3) for Right(1), Right(2)" in {
    val x: Either[String, Int] = Right(1)
    val y: Either[String, Int] = Right(2)
    Map2.map2right(x, y) {
      _ + _
    } shouldBe Right(3)
  }
  it should "return Left(very bad) for Left(very bad), Left(bad)" in {
    val x: Either[String, Int] = Left("very bad")
    val y: Either[String, Int] = Left("bad")
    Map2.map2right(x, y) {
      _ + _
    } shouldBe Left("very bad")
  }
  "map2leftRight" should "return Left(bad;very bad) for Left(very bad), Left(bad)" in {
    val x: Either[String, Int] = Left("bad")
    val y: Either[String, Int] = Left("very bad")
    Map2.map2leftRight(x, y) {
      _ + _
    } {
      _ + ";" + _
    } shouldBe Left("bad;very bad")
  }
  it should "return Right(3) for Right(1), Right(2)" in {
    val x: Either[String, Int] = Right(1)
    val y: Either[String, Int] = Right(2)
    Map2.map2leftRight(x, y) {
      _ + _
    } {
      _ + ";" + _
    } shouldBe Right(3)
  }
  it should "return Left(bad) for Right(1), Left(bad)" in {
    val x: Either[String, Int] = Right(1)
    val y: Either[String, Int] = Left("bad")
    Map2.map2leftRight(x, y) {
      _ + _
    } {
      _ + ";" + _
    } shouldBe Left("bad")
  }
  it should "return Left(bad) for Left(bad), Right(1)" in {
    val y: Either[String, Int] = Right(1)
    val x: Either[String, Int] = Left("bad")
    Map2.map2leftRight(x, y) {
      _ + _
    } {
      _ + ";" + _
    } shouldBe Left("bad")
  }
}
