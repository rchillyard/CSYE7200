package edu.neu.coe.csye7200.assthw

import edu.neu.coe.csye7200.assthw.Tries.{tryEquals, tryMatch, tryNotEquals}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.matchers.should.Matchers
import scala.util.{Failure, Success}

class TriesSpec extends AnyFlatSpec with Matchers {

  behavior of "Tries"

  it should "tryEquals" in {
    tryEquals(1, 1, "one") shouldBe Success(1)
    tryEquals(2, 1, "one").isSuccess shouldBe false
  }

  it should "tryMatch" in {
    tryMatch[Int](_ + _ == 2)(1, 1, "1 and 1") shouldBe Success(1)
    tryMatch[Int](_ + _ == 3)(1, 1, "1 and 1").isSuccess shouldBe false
  }

  it should "tryNotEquals" in {
    tryNotEquals(1, 2, "one/two") shouldBe Success(1)
    tryNotEquals(1, 1, "one/two").isSuccess shouldBe false
  }

  it should "match equal values with tryMatch" in {
    val result = Tries.tryMatch[Int](_ == _)(42, 42, "test message")
    result shouldBe Success(42)
  }

  it should "fail to match unequal values with tryMatch" in {
    val result = Tries.tryMatch[Int](_ == _)(42, 43, "test message")
    result shouldBe a[Failure[_]]
    result.failed.get.getMessage should include("test message")
  }

  it should "match equal values with tryEquals" in {
    val result = Tries.tryEquals(42, 42, "test message")
    result shouldBe Success(42)
  }

  it should "fail to match unequal values with tryEquals" in {
    val result = Tries.tryEquals(42, 43, "test message")
    result shouldBe a[Failure[_]]
    result.failed.get.getMessage should include("test message")
  }

  it should "match unequal values with tryNotEquals" in {
    val result = Tries.tryNotEquals(42, 43, "test message")
    result shouldBe Success(42)
  }

  it should "fail to match equal values with tryNotEquals" in {
    val result = Tries.tryNotEquals(42, 42, "test message")
    result shouldBe a[Failure[_]]
    result.failed.get.getMessage should include("test message")
  }

  it should "combine two successful Try values with zipTry" in {
    val try1 = Success(42)
    val try2 = Success("test")
    val result = Tries.zipTry(try1, try2)
    result shouldBe Success((42, "test"))
  }

  it should "fail when first Try is failure in zipTry" in {
    val try1 = Failure[Int](new Exception("first failure"))
    val try2 = Success("test")
    val result = Tries.zipTry(try1, try2)
    result shouldBe a[Failure[_]]
    result.failed.get.getMessage shouldBe "first failure"
  }

  it should "fail when second Try is failure in zipTry" in {
    val try1 = Success(42)
    val try2 = Failure[String](new Exception("second failure"))
    val result = Tries.zipTry(try1, try2)
    result shouldBe a[Failure[_]]
    result.failed.get.getMessage shouldBe "second failure"
  }
}