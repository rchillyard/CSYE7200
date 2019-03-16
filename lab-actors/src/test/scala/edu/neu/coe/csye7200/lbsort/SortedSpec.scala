package edu.neu.coe.csye7200.lbsort

import edu.neu.coe.csye7200.lbsort.Comparison._
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.{FlatSpec, Matchers}

import scala.language.postfixOps

/**
  * @author scalaprof
  */
class SortedSpec extends FlatSpec with Matchers with Futures with ScalaFutures {

  case class Composite(i: Int, s: String)

  object Composite {

    object OrderingCompositeInt extends Ordering[Composite] {
      def compare(x: Composite, y: Composite): Int = x.i.compare(y.i)
    }

    object OrderingCompositeString extends Ordering[Composite] {
      def compare(x: Composite, y: Composite): Int = x.s.compare(y.s)
    }
  }

  behavior of "Sorted"

  it should "sort List[Int]" in {
    val list = List(3, 1, 2)
    val sorted = Sorted(list)
    sorted() shouldBe List(1, 2, 3)
  }
  it should "sort List[String]" in {
    val list = List("b", "c", "a")
    val sorted = Sorted(list)
    sorted() shouldBe List("a", "b", "c")
  }
  it should "sort List[Double] using create" in {
    val list = List(3.0, 1.5, 2.4)
    val sorted = Sorted.create(list)
    sorted() shouldBe List(1.5, 2.4, 3.0)
  }
  it should "sort List[Char] given an explicit Comparer" in {
    val charComparer: Comparer[Char] = Ordering[Char]
    val list = List('b', 'c', 'a')
    val sorted = Sorted(list)(charComparer.invert)
    sorted() shouldBe List('c', 'b', 'a')
  }
  it should "sort List[Composite] by Int then String" in {
    val list = List(Composite(3, "c"), Composite(1, "a"), Composite(1, "z"), Composite(2, "b"))
    val comparer1: Comparer[Composite] = Composite.OrderingCompositeInt
    val comparer2: Comparer[Composite] = Composite.OrderingCompositeString
    val sorted = Sorted(list)(comparer1).sort(comparer2)
    sorted() shouldBe List(Composite(1, "a"), Composite(1, "z"), Composite(2, "b"), Composite(3, "c"))
  }
  it should "sort List[Composite] by String then Int" in {
    val list = List(Composite(3, "c"), Composite(1, "a"), Composite(1, "z"), Composite(2, "b"))
    val comparer1: Comparer[Composite] = Composite.OrderingCompositeString
    val comparer2: Comparer[Composite] = Composite.OrderingCompositeInt
    val sorted = Sorted(list)(comparer1).sort(comparer2)
    sorted() shouldBe List(Composite(1, "a"), Composite(2, "b"), Composite(3, "c"), Composite(1, "z"))
  }
  it should "sort asynchronously" in {
    import scala.concurrent.ExecutionContext.Implicits.global
    val list = List(3, 1, 2)
    val sorted = Sorted.create(list)
    val xsf = sorted.async
    whenReady(xsf) { xs => xs shouldBe List(1, 2, 3) }
  }


  behavior of "merge"
  it should "work" in {
    val l1 = List(1, 5, 8, 10, 11, 15)
    val l2 = List(3, 4, 9, 12, 14, 16)
    Sorted.merge(l1,l2) shouldBe List(1,3,4,5,8,9,10,11,12,14,15,16)
  }
}
