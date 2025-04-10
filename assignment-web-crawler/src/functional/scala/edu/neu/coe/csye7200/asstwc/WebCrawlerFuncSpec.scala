package edu.neu.coe.csye7200.asstwc

import edu.neu.coe.csye7200.asstwc.WebCrawler.{isParseableURL, fetchAndParseLinks}
import edu.neu.coe.csye7200.asstwc.fp.FP.{flatten, sequence}
import java.net.URL
import org.scalatest._
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.tagobjects.Slow
import org.scalatest.time._
import scala.util._

/**
 * @author scalaprof
 */
class WebCrawlerFuncSpec extends AnyFlatSpec with should.Matchers with Futures with ScalaFutures with TryValues with Inside {

  import scala.concurrent.ExecutionContext.Implicits.global

  val goodURL = "http://www1.coe.neu.edu/~rhillyard/indexSafe.html"
  val badURL = "http://www1.coe.neu.edu/junk"
  val malformedURL = "x//www.htmldog.com/examples/"

  def logException(x: Throwable): Unit = System.err.println(x)

  behavior of "crawl(Seq[URL])"

  it should s"succeed for $goodURL, maxHops 2" taggedAs Slow in {
    val max = 4
    val expected = 24
    val args = List(s"$goodURL")
    val uys = for (arg <- args) yield Try(new URL(arg))
    val crawler = WebCrawler(max)
    val usft = for {us <- sequence(uys)} yield crawler.crawl(us)(fetchAndParseLinks, isParseableURL)
    val usf = flatten(usft)
    whenReady(usf, timeout(Span(30, Seconds))) {
      us =>
        us.length shouldBe expected +- 1
        println(us.map(_.toString).sorted)
    }
  }
}