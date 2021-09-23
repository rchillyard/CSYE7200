package edu.neu.coe.csye7200.fp

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HashEqualsSpec extends AnyFlatSpec with Matchers {

  case class MockProduct1(x: Int)

  case class MockProduct2(x: Int, y: Any = null)

  behavior of "hashCode"
  it should "work" in {
    HashEquals.hashCode(MockProduct1(1)) shouldBe 32
    HashEquals.hashCode(MockProduct2(1, 2)) shouldBe 994
  }

  behavior of "equals"
  it should "work" in {
    HashEquals.equals(MockProduct1(1), MockProduct1(1)) shouldBe true
    HashEquals.equals(MockProduct2(1, 2), MockProduct2(1, 2)) shouldBe true
    HashEquals.equals(MockProduct1(1), MockProduct2(1, 2)) shouldBe false
    HashEquals.equals(MockProduct2(1), MockProduct2(1)) shouldBe true
  }
}

object HashEquals {
  def hashCode(p: Product): Int = p.productIterator.foldLeft(1)(_ * 31 + hashIt(_))

  def equals(p: Product, q: Product): Boolean = q.canEqual(p) && p.productIterator.sameElements(q.productIterator)

  //  def equals(p: Product, q: Product): Boolean = q.canEqual(p) && p.productArity==q.productArity && (p.productIterator zip q.productIterator).forall(x => x._1==x._2)
  private def hashIt(x: Any): Int = x match {
    case p: Product => hashCode(p)
    case null => 0
    case q => q.hashCode()
  }

}

//object HashEquals {
//  import ProductHelper._
//  def hashCode(p: Product): Int = p._hashCode
//  def equals(x: Product, y: Product): Boolean = p._equals(q)
//}
