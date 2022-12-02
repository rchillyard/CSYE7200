package edu.neu.coe.csye7200.asstll

import org.scalatest.flatspec
import org.scalatest.matchers.should
import scala.language.postfixOps

class MyLazyListSpec extends flatspec.AnyFlatSpec with should.Matchers {

  behavior of "Cons"
  it should "produce a single of 1" in {
    val x: ListLike[Int] = MyLazyList(1, () => EmptyList)
    x.head shouldBe 1
    x.tail shouldBe EmptyList
  }

  it should "produce a stream of xs using Cons directly" in {
    lazy val x: ListLike[String] = MyLazyList("x", () => x)
    val y = x.take(3).toSeq
    y.size shouldBe 3
    y.head shouldBe "x"
    y shouldBe Seq("x", "x", "x")
  }
}
