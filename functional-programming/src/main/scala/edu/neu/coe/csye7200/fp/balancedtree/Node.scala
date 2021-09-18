package edu.neu.coe.csye7200.fp.balancedtree

/**
  * Trait to define a node of a symbol table based on a tree with key type K and value type V.
  *
  * @tparam K the key type.
  * @tparam V the value type.
  */
trait Node[K, V] {
  /**
    * The degree of this Node, i.e. the number of children.
    */
  def degree: Int

  /**
    * The keys of this node.
    */
  val keys: Seq[K]

  def compare(k: K)(implicit order: Ordering[K]): (Int, Int) = {
    val kNs: LazyList[(K, Int)] = keys.to(LazyList).zipWithIndex
    val nNs: LazyList[(Int, Int)] = kNs.map { case (q, i) => Integer.signum(order.compare(k, q)) -> i }
    Node.takeUntil(nNs)(_._1 > 0).last
  }

  def find(k: K)(implicit order: Ordering[K]): (Either[Boolean, Node[K, V]], Int) = compare(k) match {
    case (0, i) => Right(this) -> i
    case (cf, i) => children(i) match {
      case Some(n) => n.find(k)
      case None => Left(cf > 0) -> i
    }
  }

  /**
    * Get the children of this node.
    *
    * @return the children as a Seq of T
    */
  def children: Seq[Option[Node[K, V]]]

  require(keys.size == degree - 1)
  require(children.size == degree)
}

object Node {
  private def takeUntil[X](xs: LazyList[X])(f: X => Boolean): LazyList[X] = {
    xs.span(f) match {
      case (h, t) => h ++ t.take(1)
    }
  }
}

abstract class BaseNode[K, V](val keys: Seq[K], val children: Seq[Option[Node[K, V]]]) extends Node[K, V]

case class TwoNode[K, V](key: K, left: Option[Node[K, V]], right: Option[Node[K, V]]) extends BaseNode[K, V](Seq(key), Seq(left, right)) {
  /**
    * The degree of this Node, i.e. the number of children.
    */
  def degree: Int = 2
}

case class ThreeNode[K, V](key1: K, key2: K, left: Option[Node[K, V]], middle: Option[Node[K, V]], right: Option[Node[K, V]]) extends BaseNode[K, V](Seq(key1, key2), Seq(left, middle, right)) {
  /**
    * The degree of this Node, i.e. the number of children.
    */
  def degree: Int = 3
}
