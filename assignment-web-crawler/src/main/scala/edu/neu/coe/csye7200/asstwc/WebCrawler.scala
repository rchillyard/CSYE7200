package edu.neu.coe.csye7200.asstwc

import edu.neu.coe.csye7200.asstwc.WebCrawler.{canParse, wget}
import java.net.{MalformedURLException, URL}
import scala.collection.immutable.Queue
import scala.concurrent._
import scala.concurrent.duration._
import scala.io.{BufferedSource, Source}
import scala.language.postfixOps
import scala.util._
import scala.util.control.NonFatal
import scala.util.matching.Regex
import scala.xml.Node

/**
 * Class to perform a web crawl.
 *
 * @param max the maximum number of recursive calls of the inner function in crawl.
 *            @param parallelism This is the number of elements we take from the queue each time in order to parallelize them.
 */
case class WebCrawler(max: Int, parallelism: Int = 8) {

    /**
     * Method to get all the URLs that can be crawled from any of the given URLs.
     *
     * @param us a list of URLs.
     * @return a Future of Seq[URL].
     */
    def crawl(us: Seq[URL])(implicit ec: ExecutionContext): Future[Seq[URL]] = {
        println(s"crawl: starting with $us")

        // NOTE: this is not tail-recursive since inner is invoked within the for comprehension.
        def inner(result: Seq[URL], queue: Queue[URL], m: Int): Future[Seq[URL]] = {
            if (result.nonEmpty) println(s"crawl: retrieved ${result.size} URLs")
            import edu.neu.coe.csye7200.asstwc.QueueOps._
            m -> queue.dequeueN(parallelism) match {
                case (0, _) =>
                    Future(result)
                case (_, (Nil, _)) =>
                    Future(result)
                case (_, (urls, qq)) =>
                    urls match {
                        case Nil => Future(result)
                        case xs =>
                            for {
                                us1 <- wget(xs)(logError)
                                uq = qq.appendedAll(us1.filter(canParse)).distinct
                                us2 = (result ++ xs).distinct
                                us3 <- inner(us2, uq, m - 1)
                            } yield us3
                    }
            }
        }

        inner(Nil, Queue().appendedAll(us), max)
    }

    def logError(x: Throwable): Unit = System.err.println(s"""crawl: ignoring exception $x ${if (x.getCause != null) " with cause " + x.getCause else ""}""")
}

/**
 * Web Crawler App.
 *
 * @author scalaprof
 */
object WebCrawler extends App {

    /**
     * The "main" Web Crawler program.
     */
    crawlWeb(args toList)

    /**
     * Main web-crawler method.
     *
     * @param ws the program arguments.
     */
    def crawlWeb(ws: Seq[String]): Unit = {
        MonadOps.sequence(ws map getURL) match {
            case Success(us) =>
                import scala.concurrent.ExecutionContext.Implicits.global
                val usf = WebCrawler(20).crawl(us)
                Await.ready(usf, Duration("90 second"))
                for (us <- usf) println(s"Links: ${us.sortBy(_.toString)}")
            case Failure(z) => println(s"Failure: $z")
        }
    }

    /**
     * Method to read the content of the given URL and return the result as a Future[String].
     *
     * @param u a URL.
     * @return a String wrapped in Future.
     */
    def getURLContent(u: URL)(implicit ec: ExecutionContext): Future[String] =
        for {
            s <- MonadOps.asFuture(SourceFromURL(u))
            w <- MonadOps.asFuture(sourceToString(s, s"Cannot read from source at $u"))
        } yield w

    /**
     * Method to validate a URL as using either HTTPS or HTTP protocol.
     *
     * CONSIDER: lift this from a URL => URL function.
     *
     * @param uy a Try[URL].
     * @return a Try[URL] which is a Success only if protocol is valid.
     */
    def validateURL(uy: Try[URL]): Try[URL] = uy.transform(u => u.getProtocol match {
        case "https" | "http" => Success(u)
        case p => Failure(WebCrawlerProtocolException(p))
    },
        x => Failure(x))

    /**
     * Method to try to predict if a URL can be read as an HTML document.
     *
     * @param u a URL.
     * @return true if the extension (or lack thereof) is appropriate.
     */
    def canParse(u: URL): Boolean = {
        val fileNameExtension: Regex = """^([\/-_\w~]*\/)?([-_\w]*)?(\.(\w*))?$""".r
        u.getPath match {
            case fileNameExtension(_, _, _, null) => true
            case fileNameExtension(_, _, _, ext) => ext match {
                case "html" | "htm" => true
                case _ => false
            }
            case _ => true
        }
    }

    /**
     * Method to get a list of URLs referenced by the given URL.
     *
     * @param url a URL.
     * @return a Future of Seq[URL] which corresponds to the various A links in the HTML.
     */
    def wget(url: URL)(implicit ec: ExecutionContext): Future[Seq[URL]] = {
        // Hint: write as a for-comprehension, using the method createURL(Option[URL], String) to get the appropriate URL for relative links
        // 16 points.
        def getURLs(ns: Node): Seq[Try[URL]] = ??? // TO BE IMPLEMENTED

        def getLinks(g: String): Try[Seq[URL]] = {
            val ny: Try[Node] = HTMLParser.parse(g) recoverWith { case f => Failure(new RuntimeException(s"parse problem with URL $url: $f")) }
            for (n <- ny; uys = getURLs(n); us <- MonadOps.sequenceForgiveSubsequent(uys) { case _: WebCrawlerProtocolException => true; case _ => false }) yield us
        }
        // Hint: write as a for-comprehension, using getURLContent (above) and getLinks above. You will also need MonadOps.asFuture
        // 9 points.
        ??? // TO BE IMPLEMENTED
    }

    /**
     * For a given list of URLs, get a (flattened) sequence of URLs by invoking wget(URL).
     *
     * @param us a list of URLs.
     * @param f a function to log an exception (which will be ignored, provided that it is non-fatal).
     * @return a Future of Seq[URL].
     */
    def wget(us: Seq[URL])(f: Throwable => Unit)(implicit ec: ExecutionContext): Future[Seq[URL]] = {
        // CONSIDER using flatten of Seq Future Seq
        val usfs: Seq[Future[Seq[URL]]] = for (u <- us) yield wget(u)
        val usyf: Future[Try[Seq[URL]]] = for {
            usys <- MonadOps.sequenceImpatient(usfs)(1000)
            q = MonadOps.sequenceForgivingWith(usys) {
                case NonFatal(x) => f(x); Success(None)
                case x => Failure(x)
            }
        } yield for (qq <- q) yield qq.flatten
        MonadOps.flatten(usyf)
    }

  private def sourceToString(source: BufferedSource, errorMsg: String): Try[String] =
    try Success(source mkString) catch {
      case NonFatal(e) => Failure(WebCrawlerURLException(errorMsg, e))
    }

  private def getURL(resource: String): Try[URL] = createURL(None, resource)

  private def createURL(context: Option[URL], resource: String): Try[URL] =
    try Success(new URL(context.orNull, resource)) catch {
      case e: MalformedURLException => Failure(WebCrawlerURLException(context.map(_.toString).getOrElse("") + s"$resource", e))
      case NonFatal(e) => Failure(WebCrawlerException(context.map(_.toString).getOrElse("") + s"$resource", e))
    }

  private def SourceFromURL(resource: URL): Try[BufferedSource] = Try(Source.fromURL(resource))
}

case class WebCrawlerURLException(url: String, cause: Throwable) extends Exception(s"Web Crawler could not decode URL: $url", cause)

case class WebCrawlerProtocolException(url: String) extends Exception(s"Web Crawler does not support protocol: $url")

case class WebCrawlerException(msg: String, cause: Throwable) extends Exception(msg, cause)

object WebCrawlerException {
  def apply(msg: String): WebCrawlerException = apply(msg, null)
}

case class Unstring(n: Int) extends CharSequence {
  def +(s: String): String = s.substring(n)

  def length(): Int = -n

  def charAt(index: Int): Char = throw UnstringException(s"charAt: $index")

  def subSequence(start: Int, end: Int): CharSequence = throw UnstringException(s"subSequence: $start, $end")
}

case class UnstringException(str: String) extends Exception(str)
