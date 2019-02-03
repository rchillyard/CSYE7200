package edu.neu.coe.csye7200

import scala.collection.LinearSeq

trait Document[-K, +V] extends (LinearSeq[K] => V) {
  def get(x: LinearSeq[K]): Option[V]

  def get(x: String)(implicit conv: String => K): Option[V] = get(x.split("""\.""").toList map conv)

  def apply(x: LinearSeq[K]): V = get(x).getOrElse(default())

  def default() = throw new NoSuchElementException

  //  def asMap[L <: K](implicit l: L): Map[L,Document[L,V]]
  def add[L <: K, W >: V](l: L, d: Document[L, W]): Document[L, W]

  def pushDown[L <: K](ls: LinearSeq[L]): Map[L, Document[L, V]]
}

case class Leaf[V](value: V) extends Document[Any, V] {
  def get(x: LinearSeq[Any]): Option[V] = x match {
    case Nil => Some(value)
    case _ => None
  }

  def asMap[L <: Any](implicit l: L): Map[L, Document[L, V]] = Map(l -> this)

  def pushDown[L <: Any](ls: LinearSeq[L]): Map[L, Document[L, V]] = ls match {
    case Nil => throw new UnsupportedOperationException("cannot pushDown with empty list")
    case h :: Nil => Map(h -> this)
    case h :: t => Map(h -> Clade(pushDown(t)))
  }

  def add[L <: Any, W >: V](l: L, d: Document[L, W]): Document[L, W] = d match {
    case Clade(x) => val z = pushDown(LinearSeq(l)); val y = z.asInstanceOf[Clade[L, W]].branches; val q = x.asInstanceOf[Map[L, Document[L, W]]]; Clade[L, W](q ++ y)
    case Leaf(_) => throw new UnsupportedOperationException("++ is not supported for two Leaf objects")
  }

  override def toString: String = value.toString
}

case class Clade[K, V](branches: Map[K, Document[K, V]]) extends Document[K, V] {
  def get(x: LinearSeq[K]): Option[V] = x match {
    case h :: t => branches.get(h) flatMap (_.get(t))
    case Nil => None
  }

  //  def asMap[L <: Any](implicit l: L): Map[L,Document[L,V]] = branches.asInstanceOf[Map[L,Document[L,V]]
  def pushDown[L <: K](ls: LinearSeq[L]): Map[L, Document[L, V]] = ls match {
    case Nil => branches.asInstanceOf[Map[L, Document[L, V]]]
    case h :: t => Map(h -> Clade(pushDown(t)))
  }

  def add[L <: K, W >: V](l: L, d: Document[L, W]): Document[L, W] = d match {
    case Clade(x) => val z = pushDown(LinearSeq(l)); val y = z.asInstanceOf[Clade[L, W]].branches; val q = x.asInstanceOf[Map[L, Document[L, W]]]; Clade[L, W](y ++ q)
    case Leaf(_) => d add(l, this.asInstanceOf[Document[L, W]])
  }

  override def toString: String = branches.toString
}

object Document {
  def apply[V](v: V): Document[Any, V] = Leaf(v)

  def apply[K, V](map: Map[K, V]): Document[K, V] = Clade(for ((k, v) <- map) yield k -> apply(v))
}
