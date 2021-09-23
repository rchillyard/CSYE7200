package edu.neu.coe.csye7200

import scala.language.implicitConversions


/**
  * Created by scalaprof on 3/22/17.
  */
case class ProductHelper(p: ProductHelperHelper) {

  def _hashCode: Int = p.apply()

  def _equals(obj: scala.Any): Boolean = obj match {
    case q: Product => p.p1.productArity == q.productArity && p.apply(q)
    case q => p.equals(q)
  }
}

/**
  * Created by scalaprof on 3/22/17.
  */
case class ProductHelperHelper(p1: Product, f1: (Any, Any) => Boolean, f2: (Int, Any) => Int, z: Int) extends (Product => Boolean) with (() => Int) {

  override def apply(): Int = p1.productIterator.foldLeft(z)(f2)

  override def apply(p2: Product): Boolean = (for (zz <- p1.productIterator zip p2.productIterator) yield f1.tupled(zz)) forall (b => b)

}

object ProductHelper {
  def apply(p: Product): ProductHelper = ProductHelper(ProductHelperHelper(p, (x, y) => x == y, _ * 31 + hashIt(_), 1))

  implicit def convert(p: Product): ProductHelper = ProductHelper(p)

  private def hashIt(x: Any): Int = x match {
    case p: Product => p._hashCode
    case q => q.hashCode()
  }
}
