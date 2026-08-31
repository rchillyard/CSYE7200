package edu.neu.coe.csye7200

import org.scalatest.{Canceled, Outcome, TestSuite}

import java.net.{InetSocketAddress, Socket}
import scala.collection.mutable
import scala.util.Try

/**
 * ScalaTest mixin that cancels a test rather than failing it when a host the test
 * needs cannot be reached.
 *
 * The companion of [[CancelOnNotImplemented]], and for the same reason: a test that
 * cannot run should be grey, not red. Red is for a mistake in the code, and someone
 * sitting on a train with no signal has not made a mistake.
 *
 * The specific case this was written for is `www1.coe.neu.edu`, which the web
 * crawler exercises use. It resolves on the Northeastern network and not from a CI
 * runner, so the same test was green on the author's machine and red on CI -- and
 * would be red for any student working from elsewhere.
 *
 * NOTE reachability is decided BEFORE the test runs, rather than by inspecting what
 * the test threw. That is deliberate: the crawler catches its own network errors and
 * carries on with an empty result, so the test that follows fails an ordinary
 * assertion ("0 was not 27 plus or minus 1") with no exception left to recognise.
 *
 * NOTE this file lives in `shared-test` and is compiled by every module that needs
 * it, so like [[CancelOnNotImplemented]] it must stay source-compatible with Scala
 * 2.12, 2.13 and 3.
 */
trait CancelWhenOffline extends TestSuite {

  /**
   * The hosts this suite needs. Bare host names, not URLs.
   */
  def requiredHosts: Seq[String]

  abstract override def withFixture(test: NoArgTest): Outcome =
    requiredHosts.find(!CancelWhenOffline.isReachable(_)) match {
      case Some(host) =>
        Canceled(s"cannot reach $host, so this test cannot run here -- not a failure of the code under test")
      case None =>
        super.withFixture(test)
    }
}

object CancelWhenOffline {

  /**
   * How long to wait for a connection before giving the host up as unreachable.
   * Short, because the answer is nearly always immediate: either DNS fails at once,
   * or the connection is made at once.
   */
  private val timeoutMillis = 4000

  /**
   * The port to try. Every host these tests use serves HTTP.
   */
  private val port = 80

  private val cache = mutable.Map.empty[String, Boolean]

  /**
   * Whether a TCP connection to the host can be opened.
   *
   * Answered once per host per JVM and remembered: a suite asks this before every
   * one of its tests, and the answer will not change during a run. Not being able
   * to connect is the answer, so every exception means false -- there is nothing
   * here worth distinguishing between an unknown host and a refused connection.
   *
   * @param host the host name to try.
   * @return true if the host accepted a connection.
   */
  def isReachable(host: String): Boolean = cache.synchronized {
    cache.getOrElseUpdate(host, {
      val socket = new Socket()
      val result = Try {
        socket.connect(new InetSocketAddress(host, port), timeoutMillis)
        true
      } getOrElse false
      Try(socket.close())
      result
    })
  }
}
