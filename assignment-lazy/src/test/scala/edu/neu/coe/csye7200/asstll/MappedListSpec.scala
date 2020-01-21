/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.asstll

import org.scalatest.{Matchers, flatspec}

class MappedListSpec extends flatspec.AnyFlatSpec with Matchers {

  behavior of "MappedList constructor"
  it should "produce a single 1" in {
    val x: ListLike[Int] = new MappedList[Int, Int](Seq(1), identity)
    x.head shouldBe 1
    x.tail shouldBe MappedList.empty
  }

  it should """produce a single "1"""" in {
    val x: ListLike[String] = new MappedList[Int, String](Seq(1), _.toString)
    x.head shouldBe "1"
    x.tail shouldBe MappedList.empty
  }


  behavior of "toSeq"
  it should "produce a single 1" in {
    val x: ListLike[Int] = MappedList[Int](1)
    x.toSeq shouldBe Seq(1)
  }
  it should "produce a sequence of 1, 2" in {
    val x = MappedList[Int](1, 2)
    x.toSeq shouldBe Seq(1, 2)
  }

  behavior of "take"
  it should "take zero" in {
    MappedList[Int](1, 2).take(0).toSeq shouldBe Nil
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

  behavior of "filter"
  it should "produce a stream of even numbers using from(1)" in {
    def even(x: Int): Boolean = x % 2 == 0

    val y = LazyList.from(1) filter even
    assert(y.head == 2)
    assert(y.tail.head == 4)
    (y take 4).toSeq shouldBe Seq(2, 4, 6, 8)
  }

  it should "produce a stream of even numbers using from(2,2)" in {
    def even(x: Int): Boolean = x % 2 == 0

    val y = LazyList.from(2, 2) filter even
    assert(y.head == 2)
    assert(y.tail.head == 4)
    (y take 4).toSeq shouldBe Seq(2, 4, 6, 8)
  }

  behavior of "filterNot"
  it should "produce a stream of even numbers using from(1)" in {
    def odd(x: Int): Boolean = x % 2 != 0

    val y = LazyList.from(1) filterNot odd
    assert(y.head == 2)
    assert(y.tail.head == 4)
    (y take 4).toSeq shouldBe Seq(2, 4, 6, 8)
  }

  it should "produce a stream of even numbers using from(2,2)" in {
    def odd(x: Int): Boolean = x % 2 != 0

    val y = LazyList.from(2, 2) filterNot odd
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

  behavior of "LazyList as a monad"
  it should "support a for-comprehension" in {
    val zs = for (x <- MyLazyList.from(1); y <- MyLazyList(Seq(1, 2, 3))) yield (x, y)
    (zs take 5).toSeq shouldBe Seq(1 -> 1, 1 -> 2, 1 -> 3, 2 -> 1, 2 -> 2)
  }

  it should "support a for-comprehension with filter" in {
    val zs = for (x <- MyLazyList.from(1); if x > 1; y <- MyLazyList(Seq(1, 2, 3)); if y == 2) yield (x, y)
    (zs take 3).toSeq shouldBe Seq(2 -> 2, 3 -> 2, 4 -> 2)
  }
}
