package edu.neu.coe.csye7200

import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.language.postfixOps

class CollectionSpec extends AnyFlatSpec with Matchers with Futures with ScalaFutures {

  import scala.concurrent.ExecutionContext.Implicits.global

  behavior of "Collection"

  it should "size" in {
    val target = Collection(List(1, 2, 3))
    val f = target.sizeAsync
    whenReady(f) {
      x => x shouldBe 3
    }
  }

  it should "listAsync" in {
    val target = Collection(List(1, 2, 3))
    val f = target.listAsync
    whenReady(f) {
      xs => xs shouldBe List(1, 2, 3)
    }
  }

  it should "map" in {
    val target = Collection(List(1, 2, 3)) map (_.toString)
    val f = target.listAsync
    whenReady(f) {
      xs => xs shouldBe List("1", "2", "3")
    }
  }

  it should "aggregateAsync" in {
    val target = Collection(List(1, 2, 3))
    val f = target.aggregateAsync(0)(_ + _)
    whenReady(f) {
      x => x shouldBe 6
    }
  }
  it should "reduceAsync" in {
    val target = Collection(List(1, 2, 3))
    val f = target.reduceAsync(_ + _)
    whenReady(f) {
      x => x shouldBe 6
    }
  }

  it should "apply" in {
    val target = Collection(List(1, 2, 3))
    target match {
      case c: LazyCollectionOnceOnly[_, _] =>
        val iterator = c.iterator
        iterator.next() shouldBe 1
        iterator.next() shouldBe 2
        iterator.next() shouldBe 3
        iterator.hasNext shouldBe false
    }
  }

}