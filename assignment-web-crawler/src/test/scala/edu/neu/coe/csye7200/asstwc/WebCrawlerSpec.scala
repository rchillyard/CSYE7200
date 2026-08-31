package edu.neu.coe.csye7200.asstwc

import edu.neu.coe.csye7200.{CancelOnNotImplemented, CancelWhenOffline}
import edu.neu.coe.csye7200.asstwc.WebCrawler.{fetchAndParseLinks, isParseableURL}
import edu.neu.coe.csye7200.asstwc.fp.FP.flatten
import edu.neu.coe.csye7200.asstwc.fp.{Crawler, FP}
import java.net.{MalformedURLException, URL}
import org.scalatest.*
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.tagobjects.Slow
import org.scalatest.time.*
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration.{Duration, DurationInt}
import scala.util.*
import scala.util.control.NonFatal

/**
 * @author scalaprof
 */
class WebCrawlerSpec extends AnyFlatSpec with should.Matchers with Futures with ScalaFutures with TryValues with Inside with CancelOnNotImplemented with CancelWhenOffline {

  // NOTE www1.coe.neu.edu resolves on the Northeastern network and not from a CI
  // runner, so without this the whole spec is red anywhere else. Note that tagging
  // is not enough on its own: "filterAndFlatten should work" below is untagged, and
  // fails an ordinary assertion because the crawler swallows the network error and
  // returns nothing.
  def requiredHosts: Seq[String] = Seq("www1.coe.neu.edu", "www.google.com")


  import scala.concurrent.ExecutionContext.Implicits.global

  val goodURL = "http://www1.coe.neu.edu/~rhillyard/indexSafe.html"
  val badURL = "http://www1.coe.neu.edu/junk"
  val malformedURL = "x//www.htmldog.com/examples/"

  def logException(x: Throwable): Unit = System.err.println(x)

  "fetchURLContent" should s"succeed for $goodURL" taggedAs Slow in {
    val wf = WebCrawler.fetchURLContent(new URL(goodURL))
    whenReady(wf, timeout(Span(6, Seconds))) { w => w.length / 100 shouldBe 50 }
  }

  "acquireAll(URL)" should s"succeed for $goodURL" taggedAs Slow in {
    val usfy = for {u <- Try(new URL(goodURL))} yield WebCrawler.fetchAndParseLinks(u)
    futureOrCancelWith(flatten(usfy), 6.second) { us => us.length shouldBe 28 }
  }

  it should s"not succeed for $badURL" taggedAs Slow in {
    val usfy = for {u <- Try(new URL(badURL))} yield WebCrawler.fetchAndParseLinks(u)
    val usf = FP.flatten(usfy)
    assertFutureThrowsOrCancel[java.io.FileNotFoundException](usf, 6.seconds)
  }

  it should s"not succeed for $goodURL" taggedAs Slow in {
    val usfy = for {u <- Try(new URL(malformedURL))} yield WebCrawler.fetchAndParseLinks(u)
    usfy.failure.exception shouldBe a[MalformedURLException]
    usfy.failure.exception should have message "no protocol: x//www.htmldog.com/examples/"
  }

  behavior of "acquireAll(Seq[URL])"
  it should s"succeed for $goodURL, https://www.google.com/" taggedAs Slow in {
    given URLordering: Ordering[URL] =
      (x: URL, y: URL) => x.getPath.compare(y.getPath)

    val ws = List(goodURL, "https://www.google.com/")
    val uys = for (w <- ws) yield Try(new URL(w))
    tryOrCancelWith(FP.sequence(uys)) {
      us =>
        val usf: Future[Seq[URL]] = Crawler.acquireAll(us)(WebCrawler.fetchAndParseLinks) {
          case NonFatal(x) =>
            System.err.println(s"ignored error: $x")
          case x =>
            ()
        }
        futureOrCancelWith(usf) {
          us =>
            us.size - 33 >= 0 shouldBe true
        }
    }
  }

  it should s"succeed for $goodURL" taggedAs Slow in {
    given URLordering: Ordering[URL] = (x: URL, y: URL) => x.getPath.compare(y.getPath)

    val ws = List(goodURL)
    val uys = for (w <- ws) yield Try(new URL(w))
    val usfy: Try[Future[Seq[URL]]] = for {us <- FP.sequence(uys)} yield Crawler.acquireAll(us)(WebCrawler.fetchAndParseLinks)(logException)
    val usf: Future[Seq[URL]] = FP.flatten(usfy)
    futureOrCancelWith(usf) {
      us =>
        println(us)
        us should not be empty
    }
  }

  behavior of "doMain"
  it should "work" in {
    val usy = new WebCrawler(1).doMain(List(goodURL))
    tryOrCancelWith(usy) {
      us => us should have size 1
    }
  }

  behavior of "filterAndFlatten"
  it should "work" taggedAs Slow in {
    given URLordering: Ordering[URL] = (x: URL, y: URL) => x.getPath.compare(y.getPath)

    val ws = List(goodURL)
    val uys = for (w <- ws) yield Try(new URL(w))
    FP.sequence(uys) match {
      case Success(us1) =>
        val exceptions = mutable.ArrayDeque[Throwable]()
        val usf: Future[Seq[URL]] = Crawler.acquireAll(us1)(fetchAndParseLinks)(x => exceptions += x)
        whenReady(usf, timeout(Span(12, Seconds))) {
          us2 =>
            us2.size shouldBe 27 +- 1
            exceptions should have size 0
        }
      case f@_ => fail(f.toString)
    }
  }

  behavior of "crawl(Seq[URL])"

  it should s"succeed for $goodURL, maxHops 2" taggedAs Slow in {
    val max = 2
    val expected = 9
    val args = List(s"$goodURL")
    val uys = for (arg <- args) yield Try(new URL(arg))
    val usft = for {us <- FP.sequence(uys)} yield WebCrawler(max).crawl(us)(WebCrawler.fetchAndParseLinks, isParseableURL)
    val usf = FP.flatten(usft)
    futureOrCancelWith(usf, 30.second) {
      us =>
        println(us.map(_.toString).sorted)
        us.length shouldBe expected +- 1
    }
  }

  behavior of "Unstring"
  it should "ignore Unstring(0)" in {
    Unstring(0) + "Hello" shouldBe "Hello"
  }
  it should "Unstring(1) gobble one character" in {
    Unstring(1) + "*Hello" shouldBe "Hello"
  }
  it should "Unstring(1) fail when written backwards" in {
    an[IndexOutOfBoundsException] shouldBe thrownBy("*Hello" + Unstring(1))
  }
}