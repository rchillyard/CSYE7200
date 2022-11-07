package edu.neu.coe.csye7200.asstwc

import java.net.{MalformedURLException, URL}
import org.scalatest._
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.tagobjects.Slow
import org.scalatest.time._
import scala.collection.mutable
import scala.concurrent.Future
import scala.util._
import scala.util.control.NonFatal

/**
  * @author scalaprof
  */
class WebCrawlerSpec extends AnyFlatSpec with should.Matchers with Futures with ScalaFutures with TryValues with Inside {

    import scala.concurrent.ExecutionContext.Implicits.global
    val goodURL = "https://www1.coe.neu.edu/~rhillyard/indexSafe.html"
    val badURL = "https://www1.coe.neu.edu/junk"
    val malformedURL = "x//www.htmldog.com/examples/"

    def logException(x: Throwable): Unit = System.err.println(x)

    "getURLContent" should s"succeed for $goodURL" taggedAs Slow in {
        val wf = WebCrawler.getURLContent(new URL(goodURL))
        whenReady(wf, timeout(Span(6, Seconds))) { w => w.length / 100 shouldBe 50 }
    }

    "wget(URL)" should s"succeed for $goodURL" taggedAs Slow in {
        val usfy = for {u <- Try(new URL(goodURL))} yield WebCrawler.wget(u)
        whenReady(MonadOps.flatten(usfy), timeout(Span(6, Seconds))) { us => us.length shouldBe 33 }
    }

    it should s"not succeed for $badURL" taggedAs Slow in {
        val usfy = for {u <- Try(new URL(badURL))} yield WebCrawler.wget(u)
        val usf = MonadOps.flatten(usfy)
        whenReady(usf.failed, timeout(Span(6, Seconds))) { e => e shouldBe a[java.io.FileNotFoundException] }
    }

    it should s"not succeed for $goodURL" taggedAs Slow in {
        val usfy = for {u <- Try(new URL(malformedURL))} yield WebCrawler.wget(u)
        usfy.failure.exception shouldBe a[MalformedURLException]
        usfy.failure.exception should have message "no protocol: x//www.htmldog.com/examples/"
    }

    behavior of "wget(Seq[URL])"
    it should s"succeed for $goodURL, https://www.google.com/" taggedAs Slow in {
        val ws = List(goodURL, "https://www.google.com/")
        val uys = for (w <- ws) yield Try(new URL(w))
        val usesfy: Try[Future[Seq[URL]]] = for {us <- MonadOps.sequence(uys)} yield WebCrawler.wget(us) {
            case NonFatal(x) => System.err.println(s"ignored error: $x"); Success(None)
            case x => Success(Some(x))
        }
        val usesf: Future[Seq[URL]] = MonadOps.flatten(usesfy)
        whenReady(usesf, timeout(Span(12, Seconds))) { us =>
            us.size shouldBe 33
        }
    }

    behavior of "wget(Seq[URL])"
    it should s"succeed for $goodURL" taggedAs Slow in {
        val ws = List(goodURL)
        val uys = for (w <- ws) yield Try(new URL(w))
        val usesfy: Try[Future[Seq[URL]]] = for {us <- MonadOps.sequence(uys)} yield WebCrawler.wget(us)(logException)
        val usesf: Future[Seq[URL]] = MonadOps.flatten(usesfy)
        whenReady(usesf, timeout(Span(12, Seconds))) { uses =>
            println(uses)
        }
    }

    behavior of "filterAndFlatten"
    it should "work" taggedAs Slow in {
        val ws = List(goodURL)
        val uys = for (w <- ws) yield Try(new URL(w))
        MonadOps.sequence(uys) match {
            case Success(us1) =>
                val exceptions = mutable.ArrayDeque[Throwable]()
                val usf: Future[Seq[URL]] = WebCrawler.wget(us1)(x => exceptions += x)
                whenReady(usf, timeout(Span(12, Seconds))) {
                    us2 =>
                        us2.distinct.size shouldBe 32
                        exceptions.size shouldBe 0
                }
            case f@_ => fail(f.toString)
        }
    }

    behavior of "crawl(Seq[URL])"

    it should s"succeed for $goodURL, max 4" taggedAs Slow in {
        val max = 4
        val expected = 25
        val args = List(s"$goodURL")
        val uys = for (arg <- args) yield Try(new URL(arg))
        val usft = for {us <- MonadOps.sequence(uys)} yield WebCrawler(max).crawl(us)
        val usf = MonadOps.flatten(usft)
        whenReady(usf, timeout(Span(20, Seconds))) { s => Assertions.assert(s.length == expected) }
    }

    behavior of "Unstring"
    it should "ignore Unstring(0)" in {
        Unstring(0) + "Hello" shouldBe "Hello"
    }
    it should "Unstring(1) gobble one character" in {
        Unstring(1) + "*Hello" shouldBe "Hello"
    }
}
