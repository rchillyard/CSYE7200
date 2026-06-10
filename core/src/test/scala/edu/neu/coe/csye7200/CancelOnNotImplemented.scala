package edu.neu.coe.csye7200

import org.scalatest.{Canceled, Failed, Outcome, TestSuite}
import org.scalatest.Assertions.{cancel, fail, succeed}
import org.scalatest.exceptions.TestFailedException
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/**
 * ScalaTest mixin that converts NotImplementedError (i.e. ???) into a
 * Canceled outcome rather than a Failed outcome, so that student-facing
 * test runs show grey cancellations instead of red failures for unimplemented
 * methods.
 *
 * Also provides tryOrCancel, tryOrCancelWith, futureOrCancel, futureOrCancelWith,
 * assertThrowsOrCancel, assertTryThrowsOrCancel, and assertFutureThrowsOrCancel
 * helpers to handle common test patterns where NotImplementedError may be
 * wrapped or intercepted before reaching withFixture.
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
   *
   * @param ty the `Try` instance to evaluate, which can be either a `Success` or a `Failure`.
   * @return an `org.scalatest.Assertion` indicating the result of the test: success, cancellation, or failure.
   */
  def tryOrCancel[T](ty: Try[T]): org.scalatest.Assertion = ty match {
    case Success(_)                      => succeed
    case Failure(e: NotImplementedError) => cancel(cancelMessage(e))
    case Failure(e)                      => fail(e)
  }

  /**
   * Unwraps a Try and passes the result to an assertion function g,
   * canceling if NotImplementedError, or failing for any other exception.
   *
   * @param ty the `Try` instance to evaluate, which can be either a `Success` or a `Failure`.
   * @param g the assertion function to apply to the result of the `Try`.
   * @return an `org.scalatest.Assertion` indicating the result of the test: success, cancellation, or failure.
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
   *
   * @param tf the `Future` instance to evaluate.
   * @param timeout the timeout duration for the future to complete.
   * @return an `org.scalatest.Assertion` indicating the result of the test: success, cancellation, or failure.
   */
  def futureOrCancel[T](tf: Future[T], timeout: Duration = 5.seconds): org.scalatest.Assertion =
    try { Await.result(tf, timeout); succeed }
    catch {
      case e: NotImplementedError =>
        cancel(cancelMessage(e))
      case e: java.util.concurrent.ExecutionException =>
        e.getCause match {
          case nie: NotImplementedError => cancel(cancelMessage(nie))
          case _                        => fail(e)
        }
      case e: Throwable =>
        fail(e)
    }

  /**
   * Awaits a Future and passes the result to an assertion function g,
   * serving as a replacement for whenReady. Cancels if NotImplementedError
   * (including when wrapped in ExecutionException), fails for any other exception.
   *
   * Usage: futureOrCancelWith(xf) { u => u should matchPattern { case _: Double => } }
   *
   * @param tf the `Future` instance to evaluate.
   * @param timeout the timeout duration for the future to complete.
   * @param g the assertion function to apply to the result of the `Try`.
   * @return an `org.scalatest.Assertion` indicating the result of the test: success, cancellation, or failure.
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
   * Asserts that the given expression throws an exception of type E,
   * canceling if NotImplementedError is thrown instead (directly or wrapped
   * in a TestFailedException). Replaces the "a[E] should be thrownBy" pattern.
   *
   * Usage: assertThrowsOrCancel[NoSuchElementException](last(Nil))
   */
  def assertThrowsOrCancel[E <: Throwable : ClassTag](f: => Any): org.scalatest.Assertion =
    try assertThrows[E](f)
    catch {
      case e: NotImplementedError => cancel(cancelMessage(e))
      case e: TestFailedException if Option(e.getCause).exists(_.isInstanceOf[NotImplementedError]) =>
        cancel(cancelMessage(e.getCause.asInstanceOf[NotImplementedError]))
    }

  /**
   * Evaluates the provided expression and applies the assertion function `g` to its result.
   * If a `NotImplementedError` is thrown during the evaluation, the test is canceled with
   * a corresponding message. If a `TestFailedException` caused by a `NotImplementedError` is thrown,
   * the test is also canceled with the corresponding message.
   *
   * @param t the expression to evaluate. It is passed by name, so it will only be evaluated when needed.
   * @param g the assertion function to apply to the result of the evaluated expression. The function
   *          should return an `org.scalatest.Assertion`.
   * @return an `org.scalatest.Assertion` indicating the test's result. It succeeds if `g` passes, cancels
   *         if a `NotImplementedError` (or related `TestFailedException`) is encountered, or fails otherwise.
   */
  def assertOrCancelWith[T](t: => T)(g: T => org.scalatest.Assertion): org.scalatest.Assertion =
    try g(t)
    catch {
      case e: NotImplementedError => cancel(cancelMessage(e))
      case e: TestFailedException if Option(e.getCause).exists(_.isInstanceOf[NotImplementedError]) =>
        cancel(cancelMessage(e.getCause.asInstanceOf[NotImplementedError]))
    }

  /**
   * Asserts that a Try is a Failure with an exception of type E,
   * canceling if NotImplementedError.
   *
   * Usage: assertTryThrowsOrCancel[NoSuchElementException](Try(last(Nil)))
   *
   * @param ty the `Try` instance to evaluate.
   * @return an `org.scalatest.Assertion` indicating the result of the test: success, cancellation, or failure.
   */
  def assertTryThrowsOrCancel[E <: Throwable : ClassTag](ty: Try[_]): org.scalatest.Assertion = ty match {
    case Success(_)                      =>
      fail("Expected exception was not thrown")
    case Failure(e: NotImplementedError) =>
      cancel(cancelMessage(e))
    case Failure(e) if implicitly[ClassTag[E]].runtimeClass.isInstance(e) =>
      succeed
    case Failure(e) =>
      fail(s"Expected ${implicitly[ClassTag[E]].runtimeClass.getSimpleName} but got ${e.getClass.getSimpleName}")
  }

  /**
   * Asserts that a Future fails with an exception of type E,
   * canceling if NotImplementedError is thrown instead.
   *
   * Usage: assertFutureThrowsOrCancel[FileNotFoundException](usf, 6.seconds)
   *
   * @param tf the `Future` instance to evaluate.
   * @param timeout the timeout duration for the future to complete.
   * @return an `org.scalatest.Assertion` indicating the result of the test: success, cancellation, or failure.
   */
  def assertFutureThrowsOrCancel[E <: Throwable : ClassTag](tf: Future[_], timeout: Duration = 5.seconds): org.scalatest.Assertion =
    try {
      Await.result(tf, timeout)
      fail("Expected exception was not thrown")
    }
    catch {
      case e: NotImplementedError =>
        cancel(cancelMessage(e))
      case e: java.util.concurrent.ExecutionException =>
        e.getCause match {
          case nie: NotImplementedError =>
            cancel(cancelMessage(nie))
          case cause if implicitly[ClassTag[E]].runtimeClass.isInstance(cause) =>
            succeed
          case cause =>
            fail(s"Expected ${implicitly[ClassTag[E]].runtimeClass.getSimpleName} but got ${cause.getClass.getSimpleName}")
        }
      case e if implicitly[ClassTag[E]].runtimeClass.isInstance(e) =>
        succeed
      case e: Throwable =>
        fail(e)
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