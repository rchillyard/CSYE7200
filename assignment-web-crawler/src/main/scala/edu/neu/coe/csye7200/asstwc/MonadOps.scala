package edu.neu.coe.csye7200.asstwc

import java.util.concurrent.TimeoutException
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.language.postfixOps
import scala.util._
import scala.util.control.NonFatal

/**
  * @author scalaprof
  */
//noinspection ScalaDeprecation
object MonadOps {

  /**
   * Method to convert a Future of Try[X] into a Future[X].
   * This is a flattening because Future itself takes on the behavior of Try.
   *
   * @param xyf a Future of Try[X].
   * @param ec  (implicit) ExecutionContext.
   * @tparam X the underlying type.
   * @return a Future[X].
   */
  def flatten[X](xyf: Future[Try[X]])(implicit ec: ExecutionContext): Future[X] = for (xy <- xyf; x <- asFuture(xy)) yield x

  /**
   * Method to convert a Try of Future[X] into a Future[X].
   * This is a flattening because Future itself takes on the behavior of Try.
   *
   * @param xfy a Try of Future[X].
   * @tparam X the underlying type.
   * @return a Future[X].
   */
  def flatten[X](xfy: Try[Future[X]]): Future[X] =
    xfy match {
      case Success(xf) => xf
      case Failure(e) => Future.failed(e)
    }

  /**
   * Method to flatten a Seq of Future of Seq[X].
   *
   * @param xsfs a Seq of Future of Seq[X].
   * @param ec   (implicit) ExecutionContext.
   * @tparam X the underlying type.
   * @return a Future of Seq[X].
   */
  def flatten[X](xsfs: Seq[Future[Seq[X]]])(implicit ec: ExecutionContext): Future[Seq[X]] = Future.sequence(xsfs) map (_ flatten)

  /**
   * TESTME
   *
   * @param voKm a Map of K -> Option[V].
   * @tparam K the key type.
   * @tparam V the value type.
   * @return a Map of K -> V
   */
  def flatten[K, V](voKm: Map[K, Option[V]]): Map[K, V] = for ((k, vo) <- voKm; v <- vo) yield k -> v

  /**
   * Method to wrap a Try[X] inside Future. No information is lost.
   *
   * @param xy a Try[X].
   * @tparam X the underlying type.
   * @return a Future[X].
   */
  def asFuture[X](xy: Try[X]): Future[X] = xy match {
    case Success(s) => Future.successful(s)
    case Failure(e) => Future.failed(e)
  }

  /**
   * Method to convert a Try[X] into an Either[Throwable, X].
   *
   * @param xy a Try[X].
   * @tparam X the underlying type.
   * @return a Left(Throwable) or Right(X).
   */
  def asEither[X](xy: Try[X]): Either[Throwable, X] =
    xy match {
      case Success(s) => Right(s)
      case Failure(e) => Left(e)
    }

  /**
   * Sequence method to combine elements of Try.
   *
   * @param xys an Iterable of Try[X].
   * @tparam X the underlying type.
   * @return a Try of Seq[X].
   *         NOTE: that the output collection type will be Seq, regardless of the input type
   */
  def sequence[X](xys: Iterable[Try[X]]): Try[Seq[X]] =
    xys.foldLeft(Try(Seq[X]())) {
      (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
    }

  /**
   * Standard method to grant forgiveness to an exception.
   * If the exception is non-fatal, a message is printed on the error output, and Success(None) is returned.
   * If the exception is fatal, a Failure is returned.
   *
   * @tparam X the underlying type.
   * @return a PartialFunction from Throwable => Try of Option[X].
   */
  def forgiveness[X](logFunction: Throwable => Unit): PartialFunction[Throwable, Try[Option[X]]] = {
    case NonFatal(x) => logFunction(x); Success(None)
    case x => Failure(x)
  }

  /**
   * Sequence method to combine elements of Try.
   *
   * @param xys an Iterable of Try[X].
   * @tparam X the underlying type.
   * @return a Try of Seq[X].
   *         NOTE: that the output collection type will be Seq, regardless of the input type
   */
  def sequenceForgiving[X](xys: Iterable[Try[X]]): Try[Seq[X]] = {
    sequenceForgivingWith[X](xys)(forgiveness(stdForgivenessFunction("sequenceForgiving")))
  }

  /**
   * Sequence method to combine elements of Try.
   *
   * @param xys       an Iterable of Try[X].
   * @param pfFailure a partial function of type Throwable => Try of Option[X].
   * @tparam X the underlying type.
   * @return a Try of Seq[X].
   *         NOTE: that the output collection type will be Seq, regardless of the input type
   */
  def sequenceForgivingWith[X](xys: Iterable[Try[X]])(pfFailure: PartialFunction[Throwable, Try[Option[X]]]): Try[Seq[X]] =
    sequenceForgivingTransform[X](xys)(x => Success(Some(x)), pfFailure)

  /**
   * Sequence method to combine elements of Try.
   *
   * @param xys       an Iterable of Try[X].
   * @param fSuccess  a function of type X => Try of Option[X].
   * @param pfFailure a partial function of type Throwable => Try of Option[X].
   * @tparam X the underlying type.
   * @return a Try of Seq[X].
   *         NOTE: that the output collection type will be Seq, regardless of the input type
   */
  def sequenceForgivingTransform[X](xys: Iterable[Try[X]])(fSuccess: X => Try[Option[X]], pfFailure: PartialFunction[Throwable, Try[Option[X]]]): Try[Seq[X]] = {
    val xosy: Try[Seq[Option[X]]] = sequence(for (xy <- xys) yield xy.transform[Option[X]](fSuccess, pfFailure))
    for (xos <- xosy) yield xos.filter(_.isDefined).map(_.get)
  }

  /**
   * This method is similar to Future.sequenceTryToEither in that it takes a Seq[Future of X] and returns a Future of Sequence ...
   * However, the return type is actually Future of Sequence of Try of X.
   * Furthermore, this method will always wait for a specific duration (millisecs) but no longer.
   * Any unevaluated future will give rise to a Failure[TimeoutException] in the result.
   *
   * @param xfs       a Seq[Future of X].
   * @param millisecs the number of milliseconds to wait.
   * @param ec        the (implicit) ExecutionContext
   * @tparam X the underlying type of the input.
   * @return a Future of Seq[Try of X]
   */
  def sequenceImpatient[X](xfs: Seq[Future[X]])(millisecs: Int)(implicit ec: ExecutionContext): Future[Seq[Try[X]]] = {
    Thread.sleep(millisecs)
    val result: Seq[Try[X]] = xfs.foldLeft(Seq.empty[Try[X]]) {
      (a, xf) =>
        xf.value match {
          case Some(xy) => a :+ xy
          case None => a :+ Failure(new TimeoutException(s"timeout: $xf"))
        }
    }
    (Promise[Seq[Try[X]]]() complete Success(result)).future
  }

  /**
   * Sequence the Seq of Try of X into a Try of Seq of X such that if any of the input elements is a Failure,
   * then only that one (the first) will be returned as a Failure.
   * Subsequent failures will be quietly ignored.
   *
   * @param xys a Seq of Try of X.
   * @tparam X the underlying type.
   * @return a Try of Seq[X].
   */
  def sequence[X](xys: Seq[Try[X]]): Try[Seq[X]] = xys.foldLeft(Try(Seq[X]()))(combineStrict(_, _)(_ => ()))

  /**
   * Sequence the Seq of Try of X into a Try of Seq of X such that if any of the input elements is a Failure,
   * then only that one (the first) will be returned as a Failure.
   * Subsequent failures will be quietly ignored.
   *
   * @param xys a Seq of Try of X.
   * @tparam X the underlying type.
   * @return a Try of Seq[X].
   */
  def sequenceForgiveSubsequent[X](xys: Seq[Try[X]])(forgive: Throwable => Boolean): Try[Seq[X]] = xys.foldLeft(Try(Seq[X]()))(combineForgiving(_, _)(forgive, _ => ()))

  /**
   * Method to sequence a Seq of Try[X] as a Try of Seq[X] but where any Failure always results in a Failure of the result.
   *
   * @param xys                 a Seq of Try[X].
   * @param onSubsequentFailure function to deal with subsequent failures (after the first).
   * @tparam X the underlying type.
   * @return a Try of Seq[X].
   */
  def sequenceWithLogging[X](xys: Seq[Try[X]])(onSubsequentFailure: Throwable => Unit): Try[Seq[X]] = xys.foldLeft(Try(Seq[X]()))(combineStrict(_, _)(onSubsequentFailure))

  /**
   * Method to sequence a Seq of Try[X] as a Try of Seq[X] but where non-fatal exceptions are forgiven using a particular logging function.
   *
   * @param xys       a Seq of Try[X].
   * @param onFailure the logging function to deal with subsequent failures (after the first).
   * @tparam X the underlying type.
   * @return a Try of Seq[X].
   */
  def sequenceLaxWithLogging[X](xys: Seq[Try[X]])(onFailure: Throwable => Unit): Try[Seq[X]] = xys.foldLeft(Try(Seq[X]()))(combineLax(_, _)(onFailure))

  /**
   * Method to sequence a Seq of Try[X] as a Try of Seq[X] but where non-fatal exceptions are forgiven.
   *
   * @param xys a Seq of Try[X].
   * @tparam X the underlying type.
   * @return a Try of Seq[X].
   */
  def sequenceLax[X](xys: Seq[Try[X]]): Try[Seq[X]] = xys.foldLeft(Try(Seq[X]()))(combineLax(_, _)(stdForgivenessFunction("sequenceLax")))

  /**
   * Method to sequence a LazyList of Try[X] as a Try of LazyList[X].
   *
   * @param xys a LazyList of Try[X].
   * @tparam X the underlying type.
   * @return a Try of LazyList[X].
   */
  def sequence[X](xys: LazyList[Try[X]]): Try[LazyList[X]] = xys.foldLeft(Try(LazyList[X]())) {
    (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
  }

  def sequence[X](xos: Seq[Option[X]]): Option[Seq[X]] = xos.foldLeft(Option(Seq[X]())) {
    (xso, xo) => for (xs <- xso; x <- xo) yield xs :+ x
  }

  /**
   * Method to extract an Option[X] from an Either[Throwable, X].
   *
   * Hint: this one is easy: just look for a method which turns an Either into a Option.
   * 7 points.
   *
   * @param xe an Either[Throwable, X].
   * @tparam X the underlying type.
   * @return if xe is a Right(x) then Some(x) else None.
   */
  def asOption[X](xe: Either[Throwable, X]): Option[X] = ??? // TO BE IMPLEMENTED

  /**
   * Method to zip two Optional objects together.
   *
   * @param ao an Option[A].
   * @param bo an Option[B].
   * @tparam A the underlying type of ao.
   * @tparam B the underlying type of bo.
   * @return an Option[(A, B)].
   */
  def zip[A, B](ao: Option[A], bo: Option[B]): Option[(A, B)] = for (a <- ao; b <- bo) yield (a, b)

  /**
   * Method to zip two Try objects together.
   *
   * @param ay a Try[A].
   * @param by a Try[B].
   * @tparam A the underlying type of ay.
   * @tparam B the underlying type of by.
   * @return a Try[(A, B)].
   */
  def zip[A, B](ay: Try[A], by: Try[B]): Try[(A, B)] = for (a <- ay; b <- by) yield (a, b)

  /**
   * Method to zip two Future objects together.
   *
   * @param af a Future[A].
   * @param bf a Future[B].
   * @tparam A the underlying type of af.
   * @tparam B the underlying type of bf.
   * @return a Future[(A, B)].
   */
  def zip[A, B](af: Future[A], bf: Future[B])(implicit ec: ExecutionContext): Future[(A, B)] = for (a <- af; b <- bf) yield a -> b

  /**
   * Method to convert an Option[X] into a Try[X] where None will result in Failure(t).
   *
   * @param xo an Option[X].
   * @param t  the throwable to return in a Failure.
   * @tparam X the underlying type.
   * @return a Try[X].
   */
  def optionToTry[X](xo: Option[X], t: => Throwable): Try[X] = Try(xo.get).recoverWith { case _: java.util.NoSuchElementException => Failure[X](t) }


  /**
   * Method to convert an Option[X] into a Try[X] where None will result in Failure(NoSuchElementException).
   *
   * @param xo an Option[X].
   * @tparam X the underlying type.
   * @return a Try[X].
   */
  def optionToTry[X](xo: Option[X]): Try[X] = Try(xo.get)

  /**
   * An alternative means of expressing a two-element for-comprehension of two Trys.
   *
   * @param ty1 a Try[T1].
   * @param ty2 a Try[T2].
   * @param f   a (T1, T2) => U.
   * @tparam T1 the underlying type of ty1.
   * @tparam T2 the underlying type of ty1.
   * @tparam U  the underlying type of the result.
   * @return a Try[U].
   */
  def map2[T1, T2, U](ty1: Try[T1], ty2: Try[T2])(f: (T1, T2) => U): Try[U] = for {t1 <- ty1; t2 <- ty2} yield f(t1, t2)

  /**
   * Method to lift a function T => U to Try[T] => Try[U].
   *
   * @param f a T => U.
   * @tparam T the underlying type of the input.
   * @tparam U the underlying type of the output.
   * @return a Try[T] => Try[U].
   */
  def liftTry[T, U](f: T => U): Try[T] => Try[U] = _ map f

  /**
   * Method to lift a function T => U to Option[T] => Option[U].
   *
   * @param f a T => U.
   * @tparam T the underlying type of the input.
   * @tparam U the underlying type of the output.
   * @return a Option[T] => Option[U].
   */
  def liftOption[T, U](f: T => U): Option[T] => Option[U] = _ map f

  /**
   * Method to lift a function T => U to Future[T] => Future[U].
   *
   * @param f a T => U.
   * @tparam T the underlying type of the input.
   * @tparam U the underlying type of the output.
   * @return a Future[T] => Future[U].
   */
  def liftFuture[T, U](f: T => U)(implicit ec: ExecutionContext): Future[T] => Future[U] = _ map f

  /**
   * Method to output a Throwable with a prefix to the standard error.
   *
   * @param w the prefix.
   * @param x the Throwable.
   */
  def stdForgivenessFunction(w: String)(x: Throwable): Unit = System.err.println(s"$w: forgiving: $x")

  /**
   * Strict form of combine.
   * If xsy is a Failure, then the result will be the same Failure, while a successful xy will be ignored and
   * a failing xy will be passed to onFailure.
   * Subsequent Failures will be processed according to the onFailure function.
   *
   * @param xsy       a Try of Seq of X (the accumulator).
   * @param xy        a Try of X (the addend).
   * @param onFailure a function to process a failing xy if xsy is a Failure.
   * @tparam X the underlying type.
   * @return a Try of Seq of X.
   */
  private def combineStrict[X](xsy: Try[Seq[X]], xy: Try[X])(onFailure: Throwable => Unit): Try[Seq[X]] = xsy match {
    case Success(xs) =>
      xy match {
        case Success(x) => Success(xs :+ x)
        case Failure(e) => Failure(e)
      }
    case Failure(e) =>
      // TODO Merge with identical code below
      xy match {
        case Success(_) =>
        case Failure(z) => onFailure(z)
      }
      Failure(e)
  }

  /**
    * Strict form of combine.
    * If xsy is a Failure, then the result will be the same Failure, while a successful xy will be ignored and
    * a failing xy will be passed to onFailure.
    * Subsequent Failures will be processed according to the onFailure function.
    *
    * @param xsy       a Try of Seq of X (the accumulator).
    * @param xy        a Try of X (the addend).
    * @param forgive   a predicate which identifies a Throwable which can be forgiven.
    * @param onFailure a function to process a failing xy if xsy is a Failure.
    * @tparam X the underlying type.
    * @return a Try of Seq of X.
    */
  private def combineForgiving[X](xsy: Try[Seq[X]], xy: Try[X])(forgive: Throwable => Boolean, onFailure: Throwable => Unit): Try[Seq[X]] = xsy match {
    case Success(xs) =>
      xy match {
        case Success(x) => Success(xs :+ x)
        case Failure(e) => if (forgive(e)) Success(xs) else Failure(e)
      }
    case Failure(e) =>
      xy match {
        case Success(_) =>
        case Failure(z) => onFailure(z)
      }
      Failure(e)
  }

  /**
    * Lax form of combine.
    * If xsy is a Success, then the result will depend on xy:
    * If xy is a Success, then the result will be the concatenation of the underlying elements.
    * If xy is a Failure, then its exception will be processed by onFailure, and xsy will be returned unchanged.
    * a failing xy will be passed to onFailure.
    * Subsequent Failures will be processed according to the onFailure function.
    *
    * @param xsy       a Try of Seq of X (the accumulator).
    * @param xy        a Try of X (the addend).
    * @param onFailure a function to process a failing xy if xsy is a Failure.
    * @tparam X the underlying type.
    * @return a Try of Seq of X.
   */
  private def combineLax[X](xsy: Try[Seq[X]], xy: Try[X])(onFailure: Throwable => Unit): Try[Seq[X]] = xsy match {
    case Success(xs) =>
      xy match {
        case Success(x) => Success(xs :+ x)
        case Failure(e) => e match {
          case NonFatal(x) => onFailure(x); xsy
          case e => Failure(e)
        }

      }
    case Failure(e) => Failure(e)
  }
}

case class MonadOpsException(msg: String, eo: Option[Throwable] = None) extends Exception(msg, eo.orNull)
