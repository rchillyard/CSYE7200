/*
 * Copyright (c) 2018, 2020. Phasmid Software
 */

package edu.neu.coe.csye7200.asstll

import org.scalatest.matchers.should
import org.scalatest.{Matchers, flatspec}

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

  behavior of "toSeq"
  it should "produce a single 1" in {
    val x: ListLike[Int] = MyLazyList(1, () => EmptyList)
    x.toSeq shouldBe Seq(1)
  }
  it should "produce a sequence of 1, 2" in {
    val x = MyLazyList(1, () => MyLazyList(2, () => EmptyList))
    x.toSeq shouldBe Seq(1, 2)
  }

  behavior of "ones"
  it should "produce a stream of 1s" in {
    val x: ListLike[Int] = MyLazyList.ones
    val y = x.take(3).toSeq
    y.size shouldBe 3
    y.head shouldBe 1
    y shouldBe Seq(1, 1, 1)
  }

  behavior of "take"
  it should "take zero from a finite stream" in {
    MyLazyList(1).take(0).toSeq shouldBe Nil
  }

  it should "take zero from an infinite stream" in {
    MyLazyList.continually(1).take(0).toSeq shouldBe Nil
  }

  it should "take 3 from a finite stream of actual length 1" in {
    (MyLazyList(1) take 3).toSeq shouldBe Seq(1)
  }

  it should "take 3 from an infinite stream" in {
    (MyLazyList.continually(1) take 3).toSeq shouldBe Seq(1, 1, 1)
  }

  it should "take 3 from an infinite stream of 1s that counts" in {
    var count = 0
    def incrementCountAndProvideValue: Int = {
      count = count + 1
      1
    }
    val lazyList = MyLazyList.continually(incrementCountAndProvideValue) take 3
    count shouldBe 1
    lazyList.toSeq shouldBe Seq(1, 1, 1)
    // LazyList is not memorizing the elements that have been evaluated already.
    count shouldBe 4
  }

  it should "take 3 from an infinite incrementing stream that counts" in {
    var count = 0
    def incrementCountAndProvideValue: Int = {
      count = count + 1
      count
    }
    val lazyList = MyLazyList.continually(incrementCountAndProvideValue) take 3
    count shouldBe 1
    lazyList.toSeq shouldBe Seq(1, 2, 3)
    // LazyList is not memorizing the elements that have been evaluated already.
    count shouldBe 4
  }

  behavior of "drop"
  it should "work correctly" in {
    val x = MyLazyList.from(1)
    val y = x drop 3 take 3
    y.toSeq shouldBe Seq(4, 5, 6)
  }

  behavior of "++"
  it should "join two Empty streams together" in {
    val x = EmptyList
    val y = x.++(EmptyList)
    y shouldBe x
  }
  it should "join a stream with an Empty stream" in {
    val empty = EmptyList
    val ones = MyLazyList.continually(1)
    val y = ones.++(empty)
    val z = y.take(3).toSeq
    z.size shouldBe 3
    z shouldBe Seq(1, 1, 1)
  }
  it should "join an Empty stream with a stream" in {
    val x: ListLike[Int] = EmptyList.asInstanceOf[ListLike[Int]]
    val ones = MyLazyList.continually(1)
    val y = x.++(ones)
    (y take 3).toSeq shouldBe Seq(1, 1, 1)
  }

  behavior of "map"
  it should "produce a stream of 2s" in {
    lazy val x: ListLike[Int] = MyLazyList(1, () => x)
    val y = x map (_ * 2)
    assert(y.head == 2)
    assert(y.tail.head == 2)
    (y take 4).toSeq shouldBe Seq(2, 2, 2, 2)
  }

  behavior of "flatMap"
  it should "produce a stream of 2s from a single element 1" in {
    val x = MyLazyList(1)
    val y = x flatMap (z => MyLazyList.continually(z * 2))
    assert(y.head == 2)
    assert(y.tail.head == 2)
    (y take 4).toSeq shouldBe Seq(2, 2, 2, 2)
  }

  it should "produce a stream of 2s from a stream of 1s" in {
    lazy val x: ListLike[Int] = MyLazyList(1, () => x)
    val y = x flatMap (z => MyLazyList.continually(z * 2))
    assert(y.head == 2)
    assert(y.tail.head == 2)
    (y take 4).toSeq shouldBe Seq(2, 2, 2, 2)
  }

  behavior of "from"
  it should "get a Seq(2, 4, 6, 8)" in {
    val x = MyLazyList.from(2, 2)
    (x take 4 toSeq) shouldBe Seq(2, 4, 6, 8)
  }

  it should "get a Seq(1, -1, -3, -5)" in {
    val x = MyLazyList.from(1, -2)
    (x take 4 toSeq) shouldBe Seq(1, -1, -3, -5)
  }

  behavior of "filter"
  it should "produce a stream of even numbers using from(1)" in {
    def even(x: Int): Boolean = x % 2 == 0

    val y = MyLazyList.from(1) filter even
    assert(y.head == 2)
    assert(y.tail.head == 4)
    (y take 4).toSeq shouldBe Seq(2, 4, 6, 8)
  }

  it should "produce a stream of even numbers using from(2,2)" in {
    def even(x: Int): Boolean = x % 2 == 0

    val y = MyLazyList.from(2, 2) filter even
    assert(y.head == 2)
    assert(y.tail.head == 4)
    (y take 4).toSeq shouldBe Seq(2, 4, 6, 8)
  }

  behavior of "filterNot"
  it should "produce a stream of even numbers using from(1)" in {
    def odd(x: Int): Boolean = x % 2 != 0

    val y = MyLazyList.from(1) filterNot odd
    assert(y.head == 2)
    assert(y.tail.head == 4)
    (y take 4).toSeq shouldBe Seq(2, 4, 6, 8)
  }

  it should "produce a stream of even numbers using from(2,2)" in {
    def odd(x: Int): Boolean = x % 2 != 0

    val y = MyLazyList.from(2, 2) filterNot odd
    assert(y.head == 2)
    assert(y.tail.head == 4)
    (y take 4).toSeq shouldBe Seq(2, 4, 6, 8)
  }

  behavior of "zip"
  it should "zip together two empty streams" in {
    EmptyList.zip(EmptyList) shouldBe EmptyList
  }
  it should "zip together a stream and an empty stream" in {
    MyLazyList.continually(1).zip(EmptyList) shouldBe EmptyList
  }
  it should "zip together an empty stream and a stream" in {
    EmptyList.zip(MyLazyList.continually(1)) shouldBe EmptyList
  }
  it should "zip together two non-empty streams" in {
    val x = MyLazyList.from(1).zip(MyLazyList.from(2))
    x.head shouldBe(1, 2)
    x.tail.head shouldBe(2, 3)
  }

  behavior of "apply"
  it should "produce a stream of a single 1" in {
    val y = MyLazyList(1) take 3
    y.toSeq shouldBe Seq(1)
  }

  behavior of "continually"
  it should "produce a stream of 1s" in {
    val y = MyLazyList.continually(1) take 3
    y.toSeq shouldBe Seq(1, 1, 1)
  }

  it should "produce a stream of 1 thru 3" in {
    val x = MyLazyList.from(1)
    val y = x take 3
    y.toSeq shouldBe Seq(1, 2, 3)
  }

  behavior of "MyLazyList as a monad"
  it should "support a for-comprehension" in {
    val zs = for (x <- MyLazyList.from(1); y <- MyLazyList(Seq(1, 2, 3))) yield (x, y)
    (zs take 5).toSeq shouldBe Seq(1 -> 1, 1 -> 2, 1 -> 3, 2 -> 1, 2 -> 2)
  }

  it should "support a for-comprehension with filter" in {
    val zs = for (x <- MyLazyList.from(1); if x > 1; y <- MyLazyList(Seq(1, 2, 3)); if y == 2) yield (x, y)
    (zs take 3).toSeq shouldBe Seq(2 -> 2, 3 -> 2, 4 -> 2)
  }
}
