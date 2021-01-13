package edu.neu.coe.csye7200.lbsort

import edu.neu.coe.csye7200.lbsort.Comparison._
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.language.postfixOps

/**
  * @author scalaprof
  */
class ComparerSpec extends AnyFlatSpec with Matchers with Futures with ScalaFutures {

  behavior of "Comparer"

  it should "compare Ints (1)" in {
    val comparer: Comparer[Int] = Ordering[Int]
    comparer(1, 2) shouldBe less
    comparer(1, 1) shouldBe Same
    comparer(2, 1) shouldBe more
  }
  it should "evaluate operators on Int" in {
    val comparer: Comparer[Int] = Ordering[Int]
    comparer.>(1, 2) shouldBe false
    comparer.>(1, 1) shouldBe false
    comparer.>(2, 1) shouldBe true
    comparer.<(1, 2) shouldBe true
    comparer.<(1, 1) shouldBe false
    comparer.<(2, 1) shouldBe false
    comparer.<=(1, 2) shouldBe true
    comparer.<=(1, 1) shouldBe true
    comparer.<=(2, 1) shouldBe false
    comparer.>=(1, 2) shouldBe false
    comparer.>=(1, 1) shouldBe true
    comparer.>=(2, 1) shouldBe true
    comparer.==(1, 2) shouldBe false
    comparer.==(1, 1) shouldBe true
    comparer.==(2, 1) shouldBe false
    comparer.!=(1, 2) shouldBe true
    comparer.!=(1, 1) shouldBe false
    comparer.!=(2, 1) shouldBe true
  }
  it should "map with function" in {
    val comparer: Comparer[Int] = Comparer.intComparer.map(_ flip)
    comparer((1, 2)) shouldBe Comparison.more
  }
  it should "invert" in {
    val comparer = Comparer.intComparer.invert
    comparer((1, 2)) shouldBe Comparison.more
  }

  private val c1a = Composite(1, "a")
  private val c2a = Composite(2, "a")
  private val c1z = Composite(1, "z")

  it should "umMap" in {
    val comparer1a: Comparer[Composite] = implicitly[Comparer[Int]].unMap(_.i)
    val comparer1b: Comparer[Composite] = implicitly[Comparer[String]].unMap(_.s)
    val comparer: Comparer[Composite] = comparer1b orElse comparer1a
    comparer(c1a, c1z) shouldBe less
    comparer(c1a, c1z) shouldBe less
    comparer(c1a, c2a) shouldBe less
    comparer(c1z, c2a) shouldBe more
    comparer(c1a, c1a) shouldBe Same
    comparer(c2a, c1a) shouldBe more
    comparer(c1z, c1a) shouldBe more
    val comparerAlt = comparer1a orElse comparer1b
    comparerAlt(c1a, c1z) shouldBe less
    comparerAlt(c1a, c2a) shouldBe less
    comparerAlt(c1z, c2a) shouldBe less
    comparerAlt(c1a, c1a) shouldBe Same
    comparerAlt(c2a, c1a) shouldBe more
    comparerAlt(c1z, c1a) shouldBe more
  }

  it should "compose" in {
    val comparer1: Comparer[Int] = implicitly[Comparer[Int]]
    val comparer2: Comparer[String] = implicitly[Comparer[String]]
    val comparer3: Comparer[(Int, String)] = comparer1 compose comparer2
    val x: (Int, String) = Composite.unapply(c1a).get
    val y: (Int, String) = Composite.unapply(c1z).get
    comparer3(x -> y) shouldBe less
    //    comparer3(Composite(1, "a") -> Composite(1, "z")) shouldBe less


  }
  it should "compose using orElse" in {
    val comparer1: Comparer[Composite] = Composite.OrderingCompositeString
    val comparer2: Comparer[Composite] = Composite.OrderingCompositeInt
    val comparer3 = comparer1 orElse comparer2
    comparer3(c1a, c1z) shouldBe less
    comparer3(c1a, c2a) shouldBe less
    comparer3(c1z, c2a) shouldBe more
    comparer3(c1a, c1a) shouldBe Same
    comparer3(c2a, c1a) shouldBe more
    comparer3(c1z, c1a) shouldBe more
    val comparer4 = comparer2 orElse comparer1
    comparer4(c1a, c1z) shouldBe less
    comparer4(c1a, c2a) shouldBe less
    comparer4(c1z, c2a) shouldBe less
    comparer4(c1a, c1a) shouldBe Same
    comparer4(c2a, c1a) shouldBe more
    comparer4(c1z, c1a) shouldBe more
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
  private val c2b = Composite(2, "b")
  private val c3c = Composite(3, "c")
  it should "sort List[Composite] by Int then String" in {
    val list = List(c3c, c1a, c1z, c2b)
    val comparer1: Comparer[Composite] = Composite.OrderingCompositeInt
    val comparer2: Comparer[Composite] = Composite.OrderingCompositeString
    val sorted = Sorted(list)(comparer1).sort(comparer2)
    sorted() shouldBe List(c1a, c1z, c2b, c3c)
  }
  it should "sort List[Composite] by String then Int" in {
    val list = List(c3c, c1a, c1z, c2b)
    val comparer1: Comparer[Composite] = Composite.OrderingCompositeString
    val comparer2: Comparer[Composite] = Composite.OrderingCompositeInt
    val sorted = Sorted(list)(comparer1).sort(comparer2)
    sorted() shouldBe List(c1a, c2b, c3c, c1z)
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
    Sorted.merge(l1, l2) shouldBe List(1, 3, 4, 5, 8, 9, 10, 11, 12, 14, 15, 16)
  }
}

case class Composite(i: Int, s: String)

object Composite {

  object OrderingCompositeInt extends Ordering[Composite] {
    def compare(x: Composite, y: Composite): Int = x.i.compare(y.i)
  }

  object OrderingCompositeString extends Ordering[Composite] {
    def compare(x: Composite, y: Composite): Int = x.s.compare(y.s)
  }

}
