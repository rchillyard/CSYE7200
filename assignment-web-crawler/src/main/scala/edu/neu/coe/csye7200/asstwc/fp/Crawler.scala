package edu.neu.coe.csye7200.asstwc.fp

import edu.neu.coe.csye7200.asstwc.fp.Crawler.acquireAll
import edu.neu.coe.csye7200.asstwc.fp.FP._
import scala.concurrent._
import scala.language.postfixOps
import scala.util._
import scala.util.control.NonFatal

/**
 * A concurrent, generic crawler that uses breadth-first search to retrieve
 * and process objects of type `U` up to a specified depth.
 * It is designed to traverse a directed graph (which might be a tree).
 * The most obvious application is web crawling, in which case `U` is `URL`.
 *
 * @param maxHops     the maximum number of hops or depth levels to crawl during the crawling process.
 * @param parallelism the maximum number of parallel asynchronous operations allowed during crawling.
 * @tparam U the type of objects to be crawled, which must have an implicit Ordering.
 */
class Crawler[U: Ordering](maxHops: Int, parallelism: Int = 8) {

  /**
   * Executes a crawling operation (BFS) starting with the given sequence of input elements,
   * acquire additional elements asynchronously, and filter the results based on a given predicate.
   *
   * The method uses an inner recursive mechanism to manage asynchronous operations and progressively
   * refine the result set through multiple iterations up to a defined limit.
   *
   * @param us        the initial sequence of input elements to start the crawling process.
   * @param acquire   a function that takes a sequence of elements and returns a function to asynchronously
   *                  acquire additional elements. The acquire function also takes a failure handler for
   *                  managing exceptions during the process.
   * @param predicate a condition function that determines whether an element should be further processed.
   * @param ec        the implicit ExecutionContext used to handle asynchronous operations.
   * @return a Future containing the final sequence of elements that satisfy the given predicate after processing.
   */
  def crawl(us: Seq[U])(acquire: U => Future[Seq[U]], predicate: U => Boolean)(implicit ec: ExecutionContext): Future[Seq[U]] = {
    println(s"Crawl: starting with $us")

    /**
     * This method is invoked directly by the enclosing crawl method.
     * It is co-recursive with `inner2`.
     * It terminates when either:
     * - the number of `hops` remaining reaches zero; or
     * - the work component of `ur0` is empty.
     * When it terminates, it returns the result component of `ur0`, wrapped in a `Future`.
     * Otherwise, it dequeues a number of `U` elements from the work component of `ur0` and
     * (if not empty) invokes `inner2` with sequence of `U` objects.
     *
     * @param ur0  the current state of the result (a `Set[U]`) and the remaining work (a `Queue[U]`).
     * @param hops the number of hops allowed at this point.
     *             A "hop" is the following of an edge in the graph being traversed.
     * @return a `Set[U]` wrapped in a `Future`.
     */
    def inner1(ur0: ResultWork[U], hops: Int): Future[Set[U]] =
      hops match {
        case 0 => Future(ur0.result)
        case _ =>
          ur0.dequeueN(parallelism) match {
            case (Nil, _) => Future(ur0.result)
            case (us: Seq[U], ur1: ResultWork[U]) => inner2(us, ur1, hops)
          }
      }

    /**
     * This method is invoked by `inner1` and, in turn, calls `inner1` recursively.
     * It operates by acquiring (asynchronously) a sequence of new `U` objects from
     * the given sequence of `U` objects, viz., `us0`.
     * It then (not asynchronously) filters the sequence according to `predicate` and then
     * adds what's left into the queue of `ur`.
     * At this point, it logs the current size of the result.
     * Finally, it invokes `inner1` asynchronously.
     *
     * @param us0  a sequence of Us.
     * @param ur   the current state of the result (a `Set[U]`) and the remaining work (a `Queue[U]`).
     * @param hops the number of hops allowed. See inner1 for explanation.
     * @return a `Set[U]` wrapped in a `Future`.
     */
    def inner2(us0: Seq[U], ur: ResultWork[U], hops: Int): Future[Set[U]] =
      for {
        us1 <- acquireAll(us0)(acquire)(logError)
        ur1 = ur.enqueue(us1.filter(predicate)) // may still need to do distinct
        _ = if (ur1.nonEmpty) println(s"crawl.inner2: total retrieved ${ur1.size} Us")
        us3 <- inner1(ur1, hops - 1)
      } yield us3

    inner1(ResultWork(us), maxHops) map (_.toSeq)
  }

  /**
   * Executes a crawling procedure on a sequence of input elements, processing them into a result
   * of interest and handling asynchronous operations for acquiring additional data.
   *
   * @param ts               the sequence of input elements to be processed.
   * @param f                a function to transform each input element into a Try-wrapped output element.
   * @param acquire          a function that takes a sequence of processed elements and creates an asynchronous operation
   *                         for acquiring more elements, accepting a failure handler as a parameter.
   * @param predicate        a condition function to determine if an element satisfies criteria for further processing.
   * @param executionContext the implicit ExecutionContext used for handling asynchronous operations.
   * @tparam T the type of input elements in the initial sequence.
   * @return a Future containing the final sequence of processed elements that meet the defined criteria.
   */
  def doCrawl[T](ts: Seq[T])(f: T => Try[U])(acquire: U => Future[Seq[U]], predicate: U => Boolean)(implicit executionContext: ExecutionContext): Future[Seq[U]] =
    for {
      us0 <- asFuture(sequence[U](ts map f))
      us1 <- crawl(us0)(acquire, predicate)
    } yield us1

  /**
   * Logs error messages for the provided exception.
   *
   * This method outputs an error message to the standard error stream, including
   * the exception details and its cause if present. It is used to handle and log
   * exceptions encountered during the crawling process without interrupting the program flow.
   *
   * @param x the exception to be logged.
   * @return Unit as it performs a side effect of logging the error message.
   */
  private def logError(x: Throwable): Unit = println(s"""Crawler: ignoring exception $x ${if (x.getCause != null) " with cause " + x.getCause else ""}""")
}

object Crawler {

  /**
   * Attempts to acquire all derived elements from an initial sequence of elements through a provided asynchronous acquisition function.
   * Handles exceptions during acquisition using a forgiveness strategy and processes acquisitions in parallel.
   *
   * @param us      the initial sequence of elements to process.
   * @param acquire a function that takes an element and returns a Future of a sequence of derived elements.
   * @param f       a function to handle exceptions that occur during acquisition.
   * @param ec      an implicit ExecutionContext used to execute asynchronous operations.
   * @tparam U the type of elements in the sequence, which is required to have an implicit Ordering.
   * @return a Future containing the flattened sequence of successfully acquired elements from all inputs.
   */
  def acquireAll[U: Ordering](us: Seq[U])(acquire: U => Future[Seq[U]])(f: Throwable => Unit)(implicit ec: ExecutionContext): Future[Seq[U]] = {
    // XXX: First, define the forgiveness function
    val forgivenessFunction: PartialFunction[Throwable, Try[Option[Seq[U]]]] = {
      case NonFatal(x) => f(x); Success(None)
      case x => Failure(x)
    }

    // XXX: for each URL in the given sequence, spawn a future which returns a sequence of derived URLs.
    // XXX: that's to say, each URL is gotten and processed IN PARALLEL.
    val usfs = for (u <- us) yield acquire(u)
    for {
      usys <- sequenceImpatient(usfs)(1000)
      ussy = sequenceForgivingWith(usys)(forgivenessFunction)
      usy = ussy.map(_.flatten)
      us <- asFuture(usy)
    } yield us
  }
}