package edu.neu.coe.csye7200.lbsort

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.language.{implicitConversions, postfixOps}

case class Sorted[T: Comparer](ts: Seq[T]) extends (() => Seq[T]) {

  private val ct = implicitly[Comparer[T]]

  implicit val ordering: Ordering[T] = ct.toOrdering

  def sort(o: Comparer[T]): Sorted[T] = Sorted(ts)(ct orElse o)

  def apply(): Seq[T] = ts.sorted

  def async(implicit ec: ExecutionContext): Future[Seq[T]] = Future(apply())

  def parSort(implicit ec: ExecutionContext): Future[Seq[T]] = Sorted.mergeSort(ts)
}

object Sorted {
  def create[T: Ordering](ts: Seq[T]): Sorted[T] = Sorted(ts)(implicitly[Ordering[T]])

  def verify[T: Comparer](xs: Seq[T]): Boolean = xs.zip(xs.tail).forall(z => implicitly[Comparer[T]].<=(z._1, z._2))

  def parSort[T: Ordering](tst: (Seq[T], Seq[T]))(implicit ec: ExecutionContext): Future[Seq[T]] = map2(Future(tst._1.sorted), Future(tst._2.sorted))(merge)

  def mergeSort[T: Ordering](ts: Seq[T])(implicit ec: ExecutionContext): Future[Seq[T]] = parSort(ts splitAt (ts.length / 2))

  def merge[T: Ordering](ts1: Seq[T], ts2: Seq[T]): Seq[T] = {
    val ordering = implicitly[Ordering[T]]

    @tailrec def inner(r: Seq[T], xs: Seq[T], ys: Seq[T]): Seq[T] = (xs, ys) match {
      case (_, Nil) => r ++ xs
      case (Nil, _) => r ++ ys
      case (x :: xs1, y :: ys1) =>
        if (ordering.lt(x, y)) inner(r :+ x, xs1, ys)
        else inner(r :+ y, xs, ys1)
      case (_, _) => throw new Exception(s"Unmatched: ($xs, $ys)")
    }

    inner(Nil, ts1, ts2)
  }

  def map2[T: Ordering](t1f: Future[Seq[T]], t2f: Future[Seq[T]])(f: (Seq[T], Seq[T]) => Seq[T])(implicit ec: ExecutionContext): Future[Seq[T]] = for {t1 <- t1f; t2 <- t2f} yield f(t1, t2)

}

object Test extends App {

  case class Composite(i: Int, d: Double)

  object Composite {

    object OrderingCompositeInt extends Ordering[Composite] {
      def compare(x: Composite, y: Composite): Int = x.i.compare(y.i)
    }

    object OrderingCompositeDouble extends Ordering[Composite] {
      def compare(x: Composite, y: Composite): Int = x.d.compare(y.d)
    }

  }

  val comparer1: Comparer[Composite] = Composite.OrderingCompositeInt
  val comparer2: Comparer[Composite] = Composite.OrderingCompositeDouble

  val list = List(Composite(3, 1.1), Composite(1, 1.1), Composite(1, 1.2), Composite(2, 2.2), Composite(2, 2.2), Composite(1, 1.0))
  val sorted1 = Sorted(list)(comparer1).sort(comparer2)
  sorted1().foreach(println(_))
  println("Test passed? " + Sorted.verify(sorted1())(comparer1.orElse(comparer2)))
  val sorted2 = Sorted(list)(comparer2).sort(comparer1)
  sorted2().foreach(println(_))
  println("Test passed? " + Sorted.verify(sorted2())(comparer2.orElse(comparer1)))

}
