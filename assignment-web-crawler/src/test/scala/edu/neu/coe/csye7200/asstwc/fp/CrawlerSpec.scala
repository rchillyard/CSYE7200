package edu.neu.coe.csye7200.asstwc.fp

import edu.neu.coe.csye7200.asstwc.WebCrawler.{isParseableURL, createURL, fetchAndParseLinks}
import edu.neu.coe.csye7200.asstwc.fp.FP.{flatten, sequence}
import java.net.URL
import org.scalatest._
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.tagobjects.Slow
import org.scalatest.time._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util._

/**
 * @author scalaprof
 */
class CrawlerSpec extends AnyFlatSpec with should.Matchers with Futures with ScalaFutures with TryValues with Inside {

  val goodURL = "http://www1.coe.neu.edu/~rhillyard/indexSafe.html"

  behavior of "doCrawl"
  it should "work" in {
    given URLordering: Ordering[URL] = (x: URL, y: URL) => x.getPath.compare(y.getPath)
    val args = List(goodURL)
    val crawler = new Crawler[URL](1)
    import scala.concurrent.ExecutionContext.Implicits.global
    val usf = crawler.doCrawl[String](args)(createURL)(fetchAndParseLinks, isParseableURL)
    // NOTE that we do not attempt to handle any (unhandled) exceptions here:
    // let them bubble up to the caller.
    Await.result(usf, Duration("360 second")).size shouldBe 1
  }

  behavior of "crawl(Seq[URL])"
  it should s"succeed for $goodURL, maxHops 2" taggedAs Slow in {
    import scala.concurrent.ExecutionContext.Implicits.global
    given URLordering: Ordering[URL] = (x: URL, y: URL) => x.getPath.compare(y.getPath)
    val max = 2
    val expected = 9
    val args = List(s"$goodURL")
    val uys = for (arg <- args) yield Try(new URL(arg))
    val usft = for {us <- sequence(uys)} yield new Crawler(max).crawl(us)(fetchAndParseLinks, isParseableURL)
    val usf = flatten(usft)
    whenReady(usf, timeout(Span(30, Seconds))) {
      us =>
        us.length shouldBe expected +- 1
        println(us.map(_.toString).sorted)
    }
  }
}