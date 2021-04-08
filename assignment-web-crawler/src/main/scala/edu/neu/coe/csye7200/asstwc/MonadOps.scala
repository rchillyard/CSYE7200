package edu.neu.coe.csye7200.asstwc

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.util._

/**
  * @author scalaprof
  */
//noinspection ScalaDeprecation
object MonadOps {

  def flatten[X](xyf: Future[Try[X]])(implicit executor: ExecutionContext): Future[X] = for (xy <- xyf; x <- asFuture(xy)) yield x

  def flatten[X](xfy: Try[Future[X]]): Future[X] =
    xfy match {
      case Success(xf) => xf
      case Failure(e) => Future.failed(e)
    }

  def flatten[X](xsfs: Seq[Future[Seq[X]]])(implicit ec: ExecutionContext): Future[Seq[X]] = Future.sequence(xsfs) map {
    _ flatten
  }

  def flattenRecover[X](esf: Future[Seq[Either[Throwable, Seq[X]]]], f: => Throwable => Unit)(implicit executor: ExecutionContext): Future[Seq[X]] = {
    def filter(uses: Seq[Either[Throwable, Seq[X]]]): Seq[X] = {
      val uses2 = for {use <- uses; if (use match {
        case Left(x) => f(x); false;
        case _ => true
      })} yield use
      val uss = for {use <- uses2; uso = sequence(use); us <- uso} yield us
      uss flatten
    }

    for {es <- esf; e = filter(es)} yield e
  }

  def flatten[K, V](voKm: Map[K, Option[V]]): Map[K, V] = for ((k, vo) <- voKm; v <- vo) yield k -> v


  def asFuture[X](xy: Try[X]): Future[X] = xy match {
    case Success(s) => Future.successful(s)
    case Failure(e) => Future.failed(e)
  }

  def sequence[X](xy: Try[X]): Either[Throwable, X] =
    xy match {
      case Success(s) => Right(s)
      case Failure(e) => Left(e)
    }

  def sequence[X](xf: Future[X])(implicit executor: ExecutionContext): Future[Either[Throwable, X]] =
    xf transform( { s => Right(s) }, identity) recoverWith[Either[Throwable, X]] { case f => Future(Left(f)) }

  // Hint: write as a for-comprehension, using the method sequence (above).
  // 6 points.
  def mapFuture[X](xfs: Seq[Future[X]])(implicit executor: ExecutionContext): Seq[Future[Either[Throwable, X]]] = ??? // TO BE IMPLEMENTED

  /**
    * Sequence the Seq of Try of X into a Try of Seq of X such that if any of the input elements is a Failure,
    * then only that one (the first) will be returned as a Failure.
    * Subsequent failures will be quietly ignored.
    *
    * @param xys a Seq of Try of X.
    * @tparam X the underlying type.
    * @return a Try of Seq of X.
    */
  // TODO Fix deprecation
  def sequence[X](xys: Seq[Try[X]]): Try[Seq[X]] = xys.foldLeft(Try(Seq[X]()))(combineStrict(_, _)(_ => ()))

  // TODO Fix deprecation
  def sequenceWithLogging[X](xys: Seq[Try[X]])(onSubsequentFailure: Throwable => Unit): Try[Seq[X]] = xys.foldLeft(Try(Seq[X]()))(combineStrict(_, _)(onSubsequentFailure))

  // TODO Fix deprecation
  def sequence[X](xys: LazyList[Try[X]]): Try[LazyList[X]] = xys.foldLeft(Try(LazyList[X]())) {
    (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
  }

  // TODO Fix deprecation
  def sequenceForgivingWithLogging[X](xys: Seq[Try[X]])(onFailure: Throwable => Unit): Try[Seq[X]] = xys.foldLeft(Try(Seq[X]()))(combineLax(_, _)(onFailure))

  // TODO Fix deprecation
  def sequenceForgiving[X](xys: Seq[Try[X]]): Try[Seq[X]] = xys.foldLeft(Try(Seq[X]()))(combineLax(_, _)(e => System.err.println(s"ignoring exception ${e.getLocalizedMessage}")))

  // TODO Fix deprecation
  def sequence[X](xos: Seq[Option[X]]): Option[Seq[X]] = xos.foldLeft(Option(Seq[X]())) {
    (xso, xo) => for (xs <- xso; x <- xo) yield xs :+ x
  }

  // Hint: this one is a little more tricky. Remember what I mentioned about Either not being a pure monad -- it needs projecting
  // 7 points.
  def sequence[X](xe: Either[Throwable, X]): Option[X] = ??? // TO BE IMPLEMENTED

  def zip[A, B](ao: Option[A], bo: Option[B]): Option[(A, B)] = for (a <- ao; b <- bo) yield (a, b)

  def zip[A, B](ao: Try[A], bo: Try[B]): Try[(A, B)] = for (a <- ao; b <- bo) yield (a, b)

  def zip[A, B](ao: Future[A], bo: Future[B]): Future[(A, B)] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    for (a <- ao; b <- bo) yield (a, b)
  }

  def optionToTry[X](xo: Option[X], t: => Throwable): Try[X] = Try(xo.get).recoverWith { case _: java.util.NoSuchElementException => Failure[X](t) }

  def optionToTry[X](xo: Option[X]): Try[X] = Try(xo.get)

  def map2[T1, T2, U](ty1: Try[T1], ty2: Try[T2])(f: (T1, T2) => U): Try[U] = for {t1 <- ty1; t2 <- ty2} yield f(t1, t2)

  def liftTry[T, U](f: T => U): Try[T] => Try[U] = _ map f

  def liftOption[T, U](f: T => U): Option[T] => Option[U] = _ map f

  def liftFuture[T, U](f: T => U)(implicit executor: ExecutionContext): Future[T] => Future[U] = _ map f

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
        case Failure(e) => onFailure(e); xsy
      }
    case Failure(e) => Failure(new Exception(s"combineLax: logic error: Failure(${e.getLocalizedMessage})"))
  }
}
