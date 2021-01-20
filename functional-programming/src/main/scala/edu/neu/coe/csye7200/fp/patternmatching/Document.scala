package edu.neu.coe.csye7200.fp.patternmatching

sealed trait Document[K, T] {

  def get(ks: List[K]): Option[T]

  def :+(x: (List[K], Document[K, T]))(implicit k: K = null.asInstanceOf[K]): Document[K, T]

  def +:(x: (List[K], Document[K, T]))(implicit k: K = null.asInstanceOf[K]): Document[K, T]

  def ++(d: Document[K, T])(implicit k: K = null.asInstanceOf[K]): Document[K, T]

  def size: Int
}

case class SingletonDocument[K, T](t: T) extends Document[K, T] {
  def get(ks: List[K]): Option[T] = ks match {
    case Nil => Some(t)
    case _ => None
  }

  def :+(x: (List[K], Document[K, T]))(implicit k: K = null.asInstanceOf[K]): Document[K, T] = x +: this

  def +:(x: (List[K], Document[K, T]))(implicit k: K = null.asInstanceOf[K]): Document[K, T] = convertToMapDocument(k) :+ x

  private[patternmatching] def convertToMapDocument(k: K): MapDocument[K, T] = MapDocument(Map[K, Document[K, T]](k -> this))

  def ++(d: Document[K, T])(implicit k: K = null.asInstanceOf[K]): Document[K, T] = convertToMapDocument(k) ++ d

  def size: Int = 1

  override def toString: String = t.toString
}

case class MapDocument[K, T](m: Map[K, Document[K, T]]) extends Document[K, T] {

  def get(ks: List[K]): Option[T] = ks match {
    case h :: t => m(h).get(t)
    case Nil => None
  }

  def :+(x: (List[K], Document[K, T]))(implicit k: K = null.asInstanceOf[K]): Document[K, T] = x._1 match {
    case h :: Nil => println("case 1"); MapDocument(m + (h -> x._2))
    case h :: t => println("case 1"); m(h) :+ t -> x._2
    case Nil => println("case 1"); MapDocument(m + (null.asInstanceOf[K] -> x._2))
  }

  def +:(x: (List[K], Document[K, T]))(implicit k: K = null.asInstanceOf[K]): Document[K, T] = this :+ x

  def ++(d: Document[K, T])(implicit k: K = null.asInstanceOf[K]): Document[K, T] = d match {
    case MapDocument(m2) => MapDocument(m ++ m2)
    case s@SingletonDocument(_) => this :+ (List() -> s)
  }

  def size: Int = (for ((_, d) <- m) yield d.size).sum

  override def toString: String = m.toString
}

/**
  * Created by scalaprof on 9/10/16.
  */
object DocumentMain extends App {

  def ++[K, T](d1: Document[K, T], d2: Document[K, T]) = d1 ++ d2

  val t1 = SingletonDocument(42)
  println(t1)
  val t1a = t1.convertToMapDocument(null.asInstanceOf[Nothing])
  println(t1a)
  val t2 = SingletonDocument(0)
  println(t2)
  val t3 = t1a ++ t2
  println(t3)
}
