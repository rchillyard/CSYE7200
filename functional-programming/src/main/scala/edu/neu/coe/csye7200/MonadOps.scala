package edu.neu.coe.csye7200

import java.util.NoSuchElementException
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.language.postfixOps
import scala.util._

/**
  * @author scalaprof
  */
object MonadOps {

  // Hint: write as a for-comprehension, using the method asFuture (below).
  // 6 points.
  def flatten[X](xyf: Future[Try[X]])(implicit executor: ExecutionContext): Future[X] = ??? // TO BE IMPLEMENTED

  def flatten[X](xfy: Try[Future[X]]): Future[X] =
    xfy match {
      case Success(xf) => xf
      case Failure(e) => Future.failed(e)
    }

  // TODO test these two flatten signatures ?? Is this a real todo?
  def flatten[X](xoy: Try[Option[X]], t: => Throwable): Try[X] = for (xo <- xoy; x <- optionToTry(xo, t)) yield x

  def flatten[X](xoy: Try[Option[X]]): Try[X] = for (xo <- xoy; x <- optionToTry(xo)) yield x

  // Hint: write as a for-comprehension, using the method Future.sequence
  // 6 points.
  def flatten[X](xsfs: Seq[Future[Seq[X]]])(implicit ec: ExecutionContext): Future[Seq[X]] = ??? // TO BE IMPLEMENTED

  def flattenRecover[X](esf: Future[Seq[Either[Throwable, Seq[X]]]], f: => Throwable => Unit)(implicit executor: ExecutionContext): Future[Seq[X]] = {
    def filter(uses: Seq[Either[Throwable, Seq[X]]]): Seq[X] = {
      val uses2 = for {use <- uses; if (use match {
        case Left(x) => f(x); false;
        case _ => true
      })} yield use
      // CONSIDER using traverse
      val uss = for {use <- uses2; uso = sequence(use); us <- uso} yield us
      uss flatten
    }

    for {es <- esf; e = filter(es)} yield e
  }

//  def flatten[X](xfy: Try[Future[X]]): Future[X] =
//    xfy match {
//      case Success(xf) => xf
//      case Failure(e) => (Promise[X] complete (throw e)).future
//    }

    def flatten[K, V](voKm: Map[K, Option[V]]): Map[K, V] = for ((k, vo) <- voKm; v <- vo) yield k -> v

  def asFuture[X](xy: Try[X]): Future[X] = xy match {
    case Success(s) => Future.successful(s)
    case Failure(e) => Future.failed(e)
  }

  // 4 points.
  def sequence[X](xy: Try[X]): Either[Throwable, X] =
  ??? // TO BE IMPLEMENTED

  def sequence[X](xf: Future[X])(implicit executor: ExecutionContext): Future[Either[Throwable, X]] =
    xf transform( { s => Right(s) }, { f => f }) recoverWith[Either[Throwable, X]] { case f => Future(Left(f)) }

  /**
    * Sequence an Option[Future[X] to a Future[Option[X]
    *
    * @param xfo      the input
    * @param executor the (implicit) execution context
    * @tparam X the underlying type
    * @return a Future[Option[X]
    */
  def sequence[X](xfo: Option[Future[X]])(implicit executor: ExecutionContext): Future[Option[X]] = xfo match {
    case Some(xf) => xf map (Some(_))
    case None => Future.successful(None)
  }

  /**
    * Sequence a Future[Option[X] to an Option[Future[X]
    *
    * @param xof      the input
    * @param executor the (implicit) execution context
    * @tparam X the underlying type
    * @return an Option[Future[X]
    */
  def sequence[X](xof: Future[Option[X]])(implicit executor: ExecutionContext): Option[Future[X]] = {
    val p: Promise[X] = Promise.apply()
    xof.onComplete {
      case Success(Some(x)) => p.complete(Success(x))
      case Success(None) => p.complete(Failure(new NoSuchElementException))
      case Failure(x) => p.complete(Failure(x))
    }
    Some(p.future)
  }

  // Hint: write as a for-comprehension, using the method sequence (above).
  // 6 points.
  def sequence[X](xfs: Seq[Future[X]])(implicit executor: ExecutionContext): Seq[Future[Either[Throwable, X]]] = ??? // TO BE IMPLEMENTED

  def sequence[X](xys: Seq[Try[X]]): Try[Seq[X]] = xys.foldLeft(Try(Seq[X]())) {
    (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
  }

  def sequence[X](xys: LazyList[Try[X]]): Try[LazyList[X]] = xys.foldLeft(Try(LazyList[X]())) {
    (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
  }

  def sequence[X](xos: Seq[Option[X]]): Option[Seq[X]] = xos.foldLeft(Option(Seq[X]())) {
    (xso, xo) => for (xs <- xso; x <- xo) yield xs :+ x
  }

  // This one is a little more tricky. Remember what I mentioned about Either not being a pure monad -- it needs projecting
  // 7 points.
  def sequence[X](xe: Either[Throwable, X]): Option[X] = ??? // TO BE IMPLEMENTED

  def zip[A, B](ao: Option[A], bo: Option[B]): Option[(A, B)] = for (a <- ao; b <- bo) yield (a, b)

  def zip[A, B](ay: Try[A], by: Try[B]): Try[(A, B)] = for (a <- ay; b <- by) yield (a, b)

  def zip[A, B](af: Future[A], bf: Future[B])(implicit executor: ExecutionContext): Future[(A, B)] = for (a <- af; b <- bf) yield (a, b)

  def optionToTry[X](xo: Option[X], t: => Throwable): Try[X] = Try(xo.get).recoverWith { case _: java.util.NoSuchElementException => Failure[X](t) }

  def optionToTry[X](xo: Option[X]): Try[X] = Try(xo.get)

  def map2[T, U](ty1: Try[T], ty2: Try[T])(f: (T, T) => U): Try[U] = for {t1 <- ty1; t2 <- ty2} yield f(t1, t2)

  def liftTry[T, U](f: T => U): Try[T] => Try[U] = _ map f

  def liftOption[T, U](f: T => U): Option[T] => Option[U] = _ map f

  def liftFuture[T, U](f: T => U)(implicit executor: ExecutionContext): Future[T] => Future[U] = _ map f
}
