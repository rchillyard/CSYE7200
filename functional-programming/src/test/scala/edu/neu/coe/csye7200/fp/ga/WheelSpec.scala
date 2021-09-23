package edu.neu.coe.csye7200.fp.ga

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

/**
  * @author scalaprof
  */
class WheelSpec extends AnyFlatSpec with Matchers {

  class Frequencies extends mutable.HashMap[String, Int]() {
    def increment(k: String) = put(k, getOrElse(k, 0) + 1)
  }

  /*
  behavior of "lookup"
  it should "yield 0->me, 1->you, 2->exception" in {
    val wheel = Wheel.create("me"->1, "you"->1)
    val tester = new edu.neu.coe.csye7200.util.PrivateMethodTester(wheel)
    tester.invokePrivate("lookup", new Integer(0)) shouldBe "me"
    tester.invokePrivate("lookup", new Integer(1)) shouldBe "you"
    val exception = the [RuntimeException] thrownBy tester.invokePrivate("lookup", new Integer(2))
    exception.getCause shouldBe an[LogicError]
  }
  it should "yield red, blue, green, etc." in {
    val wheel = Wheel.create("red"->3, "blue"->2, "green"->4)
    val tester = new edu.neu.coe.csye7200.util.PrivateMethodTester(wheel)
    tester.invokePrivate("lookup", new Integer(0)) shouldBe "red"
    tester.invokePrivate("lookup", new Integer(1)) shouldBe "red"
    tester.invokePrivate("lookup", new Integer(2)) shouldBe "red"
    tester.invokePrivate("lookup", new Integer(3)) shouldBe "blue"
    tester.invokePrivate("lookup", new Integer(4)) shouldBe "blue"
    tester.invokePrivate("lookup", new Integer(5)) shouldBe "green"
    tester.invokePrivate("lookup", new Integer(6)) shouldBe "green"
    tester.invokePrivate("lookup", new Integer(7)) shouldBe "green"
    tester.invokePrivate("lookup", new Integer(8)) shouldBe "green"
    val exception = the [RuntimeException] thrownBy tester.invokePrivate("lookup", new Integer(9))
    exception.getCause shouldBe an[LogicError]
  }
  */

  behavior of "spin"
  it should "manage simple Boolean" in {
    val r: Random = new java.util.Random(0L)
    val wheel = Wheel.create(true -> 1, false -> 1)
    wheel.spin(r) shouldBe false
    wheel.spin(r) shouldBe false
    wheel.spin(r) shouldBe true
    wheel.spin(r) shouldBe false
    wheel.spin(r) shouldBe false
    wheel.spin(r) shouldBe true
    wheel.spin(r) shouldBe false
    wheel.spin(r) shouldBe true
    wheel.spin(r) shouldBe false
    wheel.spin(r) shouldBe false
    wheel.spin(r) shouldBe true
    wheel.spin(r) shouldBe true
    wheel.spin(r) shouldBe true
  }
  it should "yield heads or tails" in {
    val r: Random = new java.util.Random(0L)
    val wheel = Wheel.create("heads" -> 1, "tails" -> 1)
    wheel.spin(r) shouldBe "tails"
    wheel.spin(r) shouldBe "tails"
    wheel.spin(r) shouldBe "heads"
    wheel.spin(r) shouldBe "tails"
    wheel.spin(r) shouldBe "tails"
    wheel.spin(r) shouldBe "heads"
    wheel.spin(r) shouldBe "tails"
    wheel.spin(r) shouldBe "heads"
    wheel.spin(r) shouldBe "tails"
    wheel.spin(r) shouldBe "tails"
    wheel.spin(r) shouldBe "heads"
    wheel.spin(r) shouldBe "heads"
    wheel.spin(r) shouldBe "heads"
  }

  it should "yield rain, clouds or sunshine" in {
    val r: Random = new java.util.Random(0L)
    val wheel = Wheel.create("rain" -> 2, "clouds" -> 3, "sunshine" -> 5)
    wheel.spin(r) shouldBe "rain"
    wheel.spin(r) shouldBe "sunshine"
    wheel.spin(r) shouldBe "sunshine"
    wheel.spin(r) shouldBe "sunshine"
    wheel.spin(r) shouldBe "sunshine"
    wheel.spin(r) shouldBe "clouds"
    wheel.spin(r) shouldBe "rain"
    wheel.spin(r) shouldBe "rain"
    wheel.spin(r) shouldBe "sunshine"
    wheel.spin(r) shouldBe "clouds"
    wheel.spin(r) shouldBe "sunshine"
  }

  it should "yield proper relative frequencies in video poker" in {
    val r: Random = new java.util.Random()
    val wheel = Wheel.create("highcard" -> 1302540, "pair" -> 1098240, "twopair" -> 123552, "trips" -> 54912, "straight" -> 10200, "flush" -> 5108, "fullhouse" -> 3744, "quads" -> 624, "straightflush" -> 36, "royal" -> 4)
    val frequencies = new Frequencies
    for (_ <- 1 to 1000000) frequencies.increment(wheel.spin(r))
    frequencies("twopair") / 10000.0 shouldBe 5.0 +- 1
    frequencies("trips") / 10000.0 shouldBe 2.0 +- 0.7
  }

  it should "be junk" in {
    def length(xs: List[Int]): Int = {
      var l = 0
      var list = xs
      while (list.nonEmpty) {
        l += 1
        list = list.tail
      }
      l
    }

    length(Nil) shouldBe 0
  }
}
