package edu.neu.coe.csye7200.lbsort

import edu.neu.coe.csye7200.lbsort.Comparison._
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.{FlatSpec, Matchers}

import scala.language.postfixOps

/**
  * @author scalaprof
  */
class ComparerSpec extends FlatSpec with Matchers with Futures with ScalaFutures {

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
    comparer.same(Comparands(1, 2)) shouldBe false
    comparer same Comparands(1, 1) shouldBe true
    comparer same (1, 1) shouldBe true
    comparer same  (2, 1) shouldBe false
    comparer differ (1, 2) shouldBe true
    comparer differ (1, 1) shouldBe false
    comparer differ 2->1 shouldBe true
  }
  it should "andThen" in {
    val comparer: Comparer[Int] = Comparer.intComparer.andThen(_ flip)
    comparer((1, 2)) shouldBe Comparison.more
  }
  it should "invert" in {
    val comparer = Comparer.intComparer.invert
    comparer((1, 2)) shouldBe Comparison.more
  }

  private val c1a = Composite(1, "a")
  private val c2a = Composite(2, "a")
  private val c1z = Composite(1, "z")

  it should "compose using orElse" in {
    val comparer1a: Comparer[Composite] = implicitly[Comparer[Int]].compose(_.i)
    val comparer1b: Comparer[Composite] = implicitly[Comparer[String]].compose(_.s)
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

  it should "compose the complex way" in {
    val comparer1: Comparer[Int] = implicitly[Comparer[Int]]
    val comparer2: Comparer[String] = implicitly[Comparer[String]]
    val comparer3: Comparer[(Int, String)] = comparer1 compose comparer2
    val x: (Int, String) = Composite.unapply(c1a).get
    val y: (Int, String) = Composite.unapply(c1z).get
    comparer3(x -> y) shouldBe less
    //    comparer3(Composite(1, "a") -> Composite(1, "z")) shouldBe less


  }
  it should "compose using orElse (2)" in {
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

  case class Mock(x: Int) extends Comparable[Mock] {
    override def comparands(t: Mock): Comparands[Mock] = this->t
  }
  implicit val comparer: Comparer[Mock] = implicitly[Comparer[Int]] compose (_.x)
  it should "compare Mock properly" in {
    comparer(Mock(1)->Mock(2)) shouldBe less
    Mock(1) < Mock(2) shouldBe true

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
