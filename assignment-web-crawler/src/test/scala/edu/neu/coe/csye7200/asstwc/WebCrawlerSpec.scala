package edu.neu.coe.csye7200.asstwc

import java.net.{MalformedURLException, URL}
import org.scalatest._
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.tagobjects.Slow
import org.scalatest.time._
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util._

/**
  * @author scalaprof
  */
class WebCrawlerSpec extends AnyFlatSpec with should.Matchers with Futures with ScalaFutures with TryValues with Inside {

  val goodURL = "http://www1.coe.neu.edu/~rhillyard/indexSafe.html"
  val badURL = "http://www1.coe.neu.edu/junk"

  "getURLContent" should s"succeed for $goodURL" taggedAs Slow in {
    val wf = WebCrawler.getURLContent(new URL(goodURL))
    whenReady(wf, timeout(Span(6, Seconds))) { w => w.length / 100 shouldBe 50 }
  }

  "wget(URL)" should s"succeed for $goodURL" taggedAs Slow in {
    val usfy = for {u <- Try(new URL(goodURL))} yield WebCrawler.wget(u)
    whenReady(MonadOps.flatten(usfy), timeout(Span(6, Seconds))) { us => us.length shouldBe 34 }
  }

  it should s"not succeed for $badURL" taggedAs Slow in {
    val usfy = for {u <- Try(new URL(badURL))} yield WebCrawler.wget(u)
    val usf = MonadOps.flatten(usfy)
    whenReady(usf.failed, timeout(Span(6, Seconds))) { e => e shouldBe a[WebCrawlerURLException] }
  }

  it should s"not succeed for $goodURL" taggedAs Slow in {
    val usfy = for {u <- Try(new URL("x//www.htmldog.com/examples/"))} yield WebCrawler.wget(u)
    usfy.failure.exception shouldBe a[MalformedURLException]
    usfy.failure.exception should have message "no protocol: x//www.htmldog.com/examples/"
  }

  "wget(Seq[URL])" should s"succeed for $goodURL, http://www.google.com/" taggedAs Slow in {
    val ws = List(goodURL, "http://www.google.com/")
    val uys = for (w <- ws) yield Try(new URL(w))
    val usesfy = for {us <- MonadOps.sequence(uys)} yield WebCrawler.wget(us)
    val usesf = MonadOps.flatten(usesfy)
    whenReady(usesf, timeout(Span(12, Seconds))) { uses =>
      uses.size shouldBe 2
      for (use <- uses) use match {
        case Right(us) => us.length should be >= 8
        case Left(x) => System.err.println(s"ignored error: $x")
      }
    }
  }

  "filterAndFlatten" should "work" taggedAs Slow in {
    val ws = List(goodURL)
    val uys = for (w <- ws) yield Try(new URL(w))
    MonadOps.sequence(uys) match {
      case Success(us1) =>
        val usefs = WebCrawler.wget(us1)
        val exceptions = mutable.ArrayDeque[Throwable]()
        val usf = MonadOps.flattenRecover(usefs, { x => exceptions += x })
        whenReady(usf, timeout(Span(12, Seconds))) {
          us2 =>
            us2.distinct.size shouldBe 33
            exceptions.size shouldBe 0
        }
      case f@_ => fail(f.toString)
    }
  }

  "crawler(Seq[URL])" should s"succeed for $goodURL, depth 2" taggedAs Slow in {
    val args = List(s"$goodURL")
    val uys = for (arg <- args) yield Try(new URL(arg))
    val usft = for {us <- MonadOps.sequenceForgiving(uys)} yield WebCrawler.crawler(2, us)
    val usf = MonadOps.flatten(usft)
    whenReady(usf, timeout(Span(60, Seconds))) { s => Assertions.assert(s.length == 35) }
  }

  //  "crawler(Seq[URL])" should "succeed for test.html, depth 2" in {
  //    val project = "/Users/scalaprof/ScalaClass/FunctionalProgramming"
  //    val dir = "src/test/scala"
  //    val pkg = "edu/neu/coe/scala/crawler"
  //    val file = "test.html"
  //    val args = List(s"file://$project/$dir/$pkg/$file")
  //    val tries = for (arg <- args) yield Try(new URL(arg))
  //    //    println(s"tries: $tries")
  //    val usft = for {us <- MonadOps.sequence(tries)} yield WebCrawler.crawler(2, us)
  //    whenReady(MonadOps.flatten(usft), timeout(Span(20, Seconds))) { s => Assertions.assert(s.length == 2) }
  //  }
  //  it should "succeed for test.html, depth 3" in {
  //    val project = "/Users/scalaprof/ScalaClass/FunctionalProgramming"
  //    val dir = "src/test/scala"
  //    val pkg = "edu/neu/coe/scala/crawler"
  //    val file = "test.html"
  //    val args = List(s"file://$project/$dir/$pkg/$file")
  //    val tries = for (arg <- args) yield Try(new URL(arg))
  //    println(s"tries: $tries")
  //    val usft = for {us <- MonadOps.sequence(tries)} yield WebCrawler.crawler(3, us)
  //    val usf = MonadOps.flatten(usft)
  //    whenReady(usf, timeout(Span(60, Seconds))) { us =>
  //      println(us)
  //      us.size shouldBe 177
  //    }
  //  }
}
