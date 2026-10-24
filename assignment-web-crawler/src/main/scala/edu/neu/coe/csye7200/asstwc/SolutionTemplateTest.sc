import edu.neu.coe.csye7200.asstwc.*
import edu.neu.coe.csye7200.asstwc.fp.FP
import java.net.{MalformedURLException, URL}
import scala.concurrent.{ExecutionContext, Future}
import scala.io.{BufferedSource, Source}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}
import scala.xml.Node

/**
 * This worksheet needs work!
 */
object Helper {
  def createURL(context: Option[URL], resource: String): Try[URL] =
    try Success(new URL(context.orNull, resource)) catch {
      case e: MalformedURLException => Failure(WebCrawlerURLException(context.map(_.toString).getOrElse("") + s"$resource", e))
      case NonFatal(e) => Failure(WebCrawlerException(context.map(_.toString).getOrElse("") + s"$resource", e))
    }

  /**
   * Method to validate a URL as using either HTTPS or HTTP protocol.
   *
   * @param u a URL.
   * @return a Try[URL] which is a Success only if protocol is valid.
   */
  def validateURL(u: URL) = u.getProtocol match {
    case "https" | "http" => Success(u)
    case p => Failure(WebCrawlerProtocolException(p))
  }
}

/**
 * Method to get a list of URLs referenced by the given URL.
 *
 * @param url a URL.
 * @return a Future of Seq[URL] which corresponds to the various A links in the HTML.
 */
def wget(url: URL)(using ec: ExecutionContext): Future[Seq[URL]] = {
  // Hint: write as two nested for-comprehensions: the outer one (first) based on Seq, the inner (second) based on Try.
  // In the latter, use the method createURL(Option[URL], String) to get the appropriate URL for a relative link.
  // Don't forget to run it through validateURL.
  // 16 points.
  def getURLs(ns: Node): Seq[Try[URL]] =
// TO BE IMPLEMENTED 
???

  def sourceToString(source: BufferedSource, errorMsg: String): Try[String] =
    try Success(source.mkString) catch {
      case NonFatal(e) => Failure(WebCrawlerURLException(errorMsg, e))
    }

  def SourceFromURL(resource: URL): Try[BufferedSource] = Try(Source.fromURL(resource))

  /**
   * Method to read the content of the given URL and return the result as a Future[String].
   *
   * @param u a URL.
   * @return a String wrapped in Future.
   */
  def getURLContent(u: URL)(using ec: ExecutionContext): Future[String] =
    for {
      s <- FP.asFuture(SourceFromURL(u))
      w <- FP.asFuture(sourceToString(s, s"Cannot read from source at $u"))
    } yield w

  def getLinks(g: String): Try[Seq[URL]] = {
    val ny: Try[Node] = HTMLParser.parse(g) recoverWith { case f => Failure(new RuntimeException(s"parse problem with URL $url: $f")) }
    for (n <- ny; uys = getURLs(n); us <- FP.sequenceForgiveSubsequent(uys) { case _: WebCrawlerProtocolException => true; case _ => false }) yield us
  }
  // Hint: write as a for-comprehension, using fetchURLContent (above) and getLinks above. You will also need FP.asFuture
  // 9 points.

  val z =
    // TO BE IMPLEMENTED 
    ???
  z
}
