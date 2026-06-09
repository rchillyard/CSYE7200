package edu.neu.coe.csye7200

import org.scalatest.{Canceled, Failed, Outcome, TestSuite}
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

/**
 * ScalaTest mixin that converts NotImplementedError (i.e. ???) into a
 * Canceled outcome rather than a Failed outcome, so that student-facing
 * test runs show grey cancellations instead of red failures for unimplemented
 * methods.
 *
 * Also provides tryOrCancel, futureOrCancel, and futureOrCancelWith helpers
 * to unwrap Try and Future values so that NotImplementedError propagates
 * correctly through those wrappers to withFixture, returning a scalatest Assertion.
 */
trait CancelOnNotImplemented extends TestSuite {

  abstract override def withFixture(test: NoArgTest): Outcome =
    super.withFixture(test) match {
      case Failed(e: NotImplementedError) =>
        Canceled(cancelMessage(e))
      case other => other
    }

  /**
   * Unwraps a Try, returning succeed if the value is present,
   * canceling if NotImplementedError, or failing for any other exception.
   */
  def tryOrCancel[T](t: Try[T]): org.scalatest.Assertion = t match {
    case Success(_)                      => succeed
    case Failure(e: NotImplementedError) => cancel(cancelMessage(e))
    case Failure(e)                      => fail(e)
  }

  /**
   * Unwraps a Try, returning g(x) if the value is present,
   * canceling if NotImplementedError, or failing for any other exception.
   */
  def tryOrCancelWith[T](ty: Try[T])(g: T => org.scalatest.Assertion): org.scalatest.Assertion = ty match {
    case Success(t)                      => g(t)
    case Failure(e: NotImplementedError) => cancel(cancelMessage(e))
    case Failure(e)                      => fail(e)
  }

  /**
   * Awaits a Future, returning succeed if it completes successfully,
   * canceling if NotImplementedError (including when wrapped in ExecutionException),
   * or failing for any other exception.
   */
  def futureOrCancel[T](tf: Future[T], timeout: Duration = 5.seconds): org.scalatest.Assertion =
    try { Await.result(tf, timeout); succeed }
    catch {
      case e: NotImplementedError => cancel(cancelMessage(e))
      case e: java.util.concurrent.ExecutionException =>
        e.getCause match {
          case nie: NotImplementedError => cancel(cancelMessage(nie))
          case _                        => fail(e)
        }
      case e: Throwable => fail(e)
    }

  /**
   * Awaits a Future and passes the result to an assertion function g,
   * serving as a replacement for whenReady. Cancels if NotImplementedError
   * (including when wrapped in ExecutionException), fails for any other exception.
   *
   * Usage: futureOrCancelWith(xf) { u => u should matchPattern { case _: Double => } }
   */
  def futureOrCancelWith[T](tf: Future[T], timeout: Duration = 5.seconds)(g: T => org.scalatest.Assertion): org.scalatest.Assertion =
    try { val t = Await.result(tf, timeout); g(t) }
    catch {
      case e: NotImplementedError => cancel(cancelMessage(e))
      case e: java.util.concurrent.ExecutionException =>
        e.getCause match {
          case nie: NotImplementedError => cancel(cancelMessage(nie))
          case _                        => fail(e)
        }
      case e: Throwable => fail(e)
    }

  /**
   * Extracts a human-readable location from a NotImplementedError stack trace,
   * skipping scala.* frames to find the first user code location.
   */
  private def cancelMessage(e: NotImplementedError): String = {
    val location = e.getStackTrace
            .find(s => !s.getClassName.startsWith("scala."))
            .map(s => s"${s.getFileName}:${s.getLineNumber}")
            .getOrElse("unknown location")
    s"You need to implement the code at $location"
  }
}