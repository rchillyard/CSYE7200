package edu.neu.coe.csye7200.lab99.container

import edu.neu.coe.csye7200.CancelOnNotImplemented
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MyContainerSpec extends AnyFlatSpec with Matchers with CancelOnNotImplemented {

  behavior of "MyContainer"

  it should "get value" in {
    val container = MyContainer(42)
    container.get shouldBe 42
  }

  it should "map value" in {
    val container = MyContainer(42)
    container.map(x => x * 2).get shouldBe 84
  }

  it should "flatMap value" in {
    val container = MyContainer(42)
    container.flatMap(x => MyContainer(x * 2)).get shouldBe 84
  }

  it should "filter value" in {
    val container = MyContainer(42)
    container.filter(_ > 20).get shouldBe 42
//    container.filter(_ < 20) shouldBe None
  }

  it should "foreach value" in {
    val container = MyContainer(42)
    var result = 0
    container.foreach(x => result = x)
    result shouldBe 42
  }
}
