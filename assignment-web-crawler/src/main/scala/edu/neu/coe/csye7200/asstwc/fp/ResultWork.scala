package edu.neu.coe.csye7200.asstwc.fp

import edu.neu.coe.csye7200.asstwc.fp.QueueOps.RichQueue
import scala.collection.immutable.{Queue, TreeSet}

/**
 * Class which combines the result parameter and the work (queue) parameter for the inner methods of WebCrawler:crawl.
 *
 * @param result the Xs which have been successfully processed.
 * @param queue  the Xs which are still in the queue.
 */
case class ResultWork[X](result: TreeSet[X], queue: Queue[X]) {
  def dequeueN(n: Int): (Seq[X], ResultWork[X]) = {
    val (xs, xq) = queue.dequeueN(n)
    xs -> ResultWork(result ++ xs, xq)
  }

  /**
   * Enqueue elements to the queue.
   *
   * @param xs the elements to be enqueued.
   * @return a copy of this ResultWork but with the queue updated.
   */
  def enqueue(xs: Seq[X]): ResultWork[X] = {
    val xs1 = xs.distinct filterNot result.contains
    copy(queue = queue.appendedAll(xs1))
  }

  @deprecatedOverriding("nonEmpty is defined as !isEmpty; override isEmpty instead", "2.13.0")
  def nonEmpty: Boolean = !result.isEmpty

  override def toString: String = if (queue.isEmpty) result.toString else "Result in transition"

  def size: Int = result.size

  def isEmpty: Boolean = result.isEmpty

}

object ResultWork {
  def apply[X: Ordering](xs: Seq[X], xq: Queue[X]): ResultWork[X] = new ResultWork(TreeSet.from(xs), xq)

  def apply[X: Ordering](xs: Seq[X]): ResultWork[X] = apply(Nil, Queue().appendedAll(xs))
}