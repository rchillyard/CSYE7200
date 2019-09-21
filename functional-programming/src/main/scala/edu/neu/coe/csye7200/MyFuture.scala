package edu.neu.coe.csye7200

import scala.concurrent.ExecutionContext

/**
  * Created by scalaprof on 10/6/16.
  */
trait MyFuture[T] {

  def await: T

  def get: Option[T]

  def unit(t: => T)(implicit executor: ExecutionContext): MyFuture[T]

  def map[U](f: T => U)(implicit executor: ExecutionContext): MyFuture[U]

  def onComplete[U](f: T => U)(implicit executor: ExecutionContext): Unit

  def onException[U](e: Throwable => U)(implicit executor: ExecutionContext): Unit
}

case class Successful[T](t: T) extends MyFuture[T] {

  def get: Option[T] = ???

  def onComplete[U](f: T => U)(implicit executor: ExecutionContext): Unit = ???

  def onException[U](e: Throwable => U)(implicit executor: ExecutionContext): Unit = ???

  def unit(t: => T)(implicit executor: ExecutionContext): MyFuture[T] = ???

  def map[U](f: T => U)(implicit executor: ExecutionContext): MyFuture[U] = ???

  def await: T = ???
}

object MyFutureApp extends App {

  def sum(is: IndexedSeq[Int]): Int =
    if (is.size <= 1)
      is.headOption getOrElse 0
    else {
      val (l, r) = is.splitAt(is.length / 2)
      sum(l) + sum(r)
    }
}

//
//trait Par[T] {
//  def run(implicit ec: ExecutionContext): Future[T]
//  def map2(f: (T,T)=>T)(p: Par[T]): Par[T] = for (t1 <- this; t2 <- p1) yield f(t1,t2)
//}
//
//object Par {
//  def unit[T](t: => T): Par[T] = ???
//  def sequence[T](tps: List[Par[T]]): Par[List[T]] = ???
//
//  def sum(is: IndexedSeq[Int]): Int =
//    if (is.size <= 1)
//      is.headOption getOrElse 0
//    else {
//      val (l,r) = is.splitAt(is.length/2)
//      val sumL = Par.unit(sum(l))
//      val sumR = Par.unit(sum(r))
//      val result = sumL.map2(_+_)(sumR)
//      import scala.concurrent.ExecutionContext.Implicits.global
//      result.run.get
//    }
//}
