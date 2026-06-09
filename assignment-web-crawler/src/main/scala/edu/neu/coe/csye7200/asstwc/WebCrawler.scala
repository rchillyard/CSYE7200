package edu.neu.coe.csye7200.asstwc

import edu.neu.coe.csye7200.asstwc.WebCrawler.{createURL, fetchAndParseLinks, isParseableURL}
import edu.neu.coe.csye7200.asstwc.fp.FP.*
import edu.neu.coe.csye7200.asstwc.fp.{Crawler, Timer}
import java.net.{MalformedURLException, URL}
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.io.{BufferedSource, Source}
import scala.language.postfixOps
import scala.util.*
import scala.util.control.NonFatal
import scala.util.matching.Regex
import scala.xml.Node

/**
 * Represents a web crawler implementation that extends base functionality for crawling web URLs.
 * This class enables web crawling by defining the maximum number of hops (depth of traversal) and allows
 * configuration of parallelism for concurrent processing.
 * It overrides the base comparison function to sort URLs based on their paths.
 * NOTE that this class has two parts of the code that must be implemented by the student.
 *
 * @param maxHops     the maximum number of hops (depth) that the crawler will follow from the initial URL.
 * @param parallelism the level of concurrency for crawling operations, defaults to 8.
 */
case class WebCrawler(maxHops: Int, parallelism: Int = 8) extends
        Crawler[URL](maxHops, parallelism)((x: URL, y: URL) => x.getPath.compare(y.getPath)) {

  /**
   * Executes the main operation of the web crawler by processing a sequence of input strings representing URLs
   * and then passing those URLs to `doCrawl`.
   *
   * This method performs the following steps:
   * - Converts the input strings to URLs.
   * - Initiates the crawling process to retrieve additional URLs.
   * - Waits for the crawling process to complete and retrieves the result.
   * - Outputs the total number of URLs retrieved and prints each URL.
   *
   * Exceptions are not explicitly handled within this method and are allowed to propagate to the caller.
   *
   * @param args a sequence of input strings representing URLs to start the crawling process.
   * @return Unit, as this method performs a side effect of crawling and printing the results.
   */
//  def doMain(args: Seq[String]): Seq[URL] = {
//    import scala.concurrent.ExecutionContext.Implicits.global
//    val usf = doCrawl(args)(createURL)(fetchAndParseLinks, isParseableURL)
//    // NOTE that we do not attempt to handle any (unhandled) exceptions here:
//    // let them bubble up to the caller.
//    val us: Seq[URL] = Await.result(usf, Duration("360 second"))
//  }

  def doMain(args: Seq[String]): Try[Seq[URL]] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val usf = doCrawl(args)(createURL)(fetchAndParseLinks, isParseableURL)
    Try(Await.result(usf, 6.seconds)).recoverWith {
      case e: java.util.concurrent.ExecutionException =>
        Failure(Option(e.getCause).getOrElse(e))
    }
  }
}

/**
 * The `WebCrawler` object implements a basic web crawler to parse and extract URLs from web pages.
 *
 * This object leverages concurrency to crawl multiple URLs simultaneously. It includes functionality
 * to validate, fetch, and parse web page contents, extracting all reachable and parsable links in the process.
 */
object WebCrawler {

  /**
   * Fetches the content of a given URL as a string asynchronously.
   *
   * This method leverages `SourceFromURL` to open a connection to the provided URL
   * and reads its content using `sourceToString`. Both operations are wrapped
   * within a `Future` for asynchronous execution.
   *
   * @param u  the URL whose content is to be retrieved.
   * @param ec the implicit execution context used to manage asynchronous operations.
   * @return a Future containing the content of the URL as a String, or a failed Future
   *         if an error occurs during the fetch or reading of the content.
   */
  def fetchURLContent(u: URL)(using ec: ExecutionContext): Future[String] =
    for {
      s <- asFuture(SourceFromURL(u))
      w <- asFuture(sourceToString(s, s"Cannot read from source at $u"))
    } yield w

  /**
   * Validates a given URL by checking its protocol and read permissions.
   *
   * This method evaluates if the provided URL has a valid protocol (e.g., "http" or "https")
   * and if the URL can be read. If both checks pass, the URL is considered valid and returned
   * as a `Success`. Otherwise, a specific `Failure` is returned based on which check failed:
   * - `WebCrawlerProtocolException` indicates an invalid protocol.
   * - `WebCrawlerFileTypeException` indicates the URL cannot be read.
   *
   * @param u the URL to be validated.
   * @return a `Try[URL]` containing the valid URL if successful, or a `Failure` with an appropriate exception.
   */
  def validateURL(u: URL): Try[URL] =
    (validProtocol(u.getProtocol), canReadURL(u)) match {
      case (true, true) =>
        Success(u)
      case (true, _) =>
        Failure(WebCrawlerFileTypeException(u.getFile))
      case _ =>
        Failure(WebCrawlerProtocolException(u.getProtocol))
    }

  /**
   * Determines whether a given URL can be parsed based on its file extension or other conditions.
   *
   * This method checks if the provided URL has a valid file extension (e.g., "html" or "htm")
   * or other specific properties that qualify it as parsable. If the file extension is neither
   * of the acceptable values nor null, it may return false. In other cases, it assumes the URL
   * can be parsed.
   *
   * @param u the URL to be evaluated
   * @return true if the URL can be parsed, false otherwise
   */
  def isParseableURL(u: URL): Boolean = isValidExtension(notNull[URL, String](_.getPath)(u))

  /**
   * Fetches and parses the content of the given URL to extract the list of linked URLs.
   *
   * This method retrieves the content of the provided URL using `fetchURLContent`
   * and extracts all the valid links using the `getLinks` method. It returns a
   * sequence of resolved and valid links as a `Future`.
   *
   * @param url the URL to be fetched and parsed.
   * @param ec  the implicit execution context to be used for asynchronous operations.
   * @return a Future containing a sequence of resolved and valid URLs extracted from the content of the input URL.
   */
  def fetchAndParseLinks(url: URL)(using ec: ExecutionContext): Future[Seq[URL]] =
    // Hint: write as a for-comprehension, using fetchURLContent (above) and getLinks (below).
    // You will also need FP.asFuture
    // 9 points.
    // TO BE IMPLEMENTED 
    ???
  // END SOLUTION

  /**
   * Extracts and validates a list of URLs from the given HTML content string and a base URL.
   *
   * This method parses the HTML content provided as a string to extract an `XML Node` structure.
   * It then derives a sequence of URLs from this parsed node, resolving relative URLs based on the provided base URL.
   * Subsequently, it validates and filters these URLs, producing a sequence of fully resolved, valid URLs.
   *
   * @param g   an HTML content string from which to extract URLs.
   * @param url the base URL used for resolving relative links.
   * @return a `Try[Seq[URL]]` containing a sequence of valid, resolved URLs, or a `Failure` if an error occurs during parsing or validation.
   */
  def getLinks(g: String, url: URL): Try[Seq[URL]] = for {
    n <- HTMLParser.parse(g) recoverWith parseRecoveryFunction(url)
    uys = getURLs(n, url)
    us <- sequenceForgiveSubsequent(uys)(exceptionForgivenessFunction)
  } yield us

  /**
   * Extracts and validates a sequence of URLs from the HTML content of a given XML node.
   *
   * This method traverses the given XML node to find all anchor (`<a>`) elements and extracts their `href` attributes.
   * It validates the extracted `href` strings to ensure they represent valid URLs before returning them as `Try[URL]`.
   *
   * @param node the XML `Node` representing the parsed HTML content.
   * @param url  the base `URL` used to resolve relative URLs found in the `href` attributes.
   * @return a sequence of `Try[URL]` objects, representing the valid URLs extracted and resolved from the node.
   */
  def getURLs(node: Node, url: URL): Seq[Try[URL]] =
// TO BE IMPLEMENTED 
    ???
// END SOLUTION

  /**
   * Converts the content of a given BufferedSource to a String.
   *
   * This method attempts to read the content of the provided source and represents it as a String.
   * In case of a failure (such as I/O errors), it captures the exception and returns a Failure
   * with a meaningful error message.
   *
   * @param source   the BufferedSource whose content is to be converted to a String.
   * @param errorMsg the error message to include in the Failure in case of an exception.
   * @return a Try containing the String representation of the source's content, or a Failure if an error occurs.
   */
  private def sourceToString(source: BufferedSource, errorMsg: String): Try[String] =
    Try(source mkString) recoverWith urlException(errorMsg)

  /**
   * Validates a given URL string by checking if it starts with prohibited schemes or contains unsafe content.
   *
   * A valid URL string should not start with "tel:" or "mailto:", and should not contain "javascript".
   *
   * @param w the URL string to validate.
   * @return true if the URL string is valid, false otherwise.
   */
  private def isValidURLString(w: String) =
    !w.startsWith("tel:") && !w.startsWith("mailto:") && !w.contains("javascript")

  /**
   * Determines the validity of a file extension from a string wrapped in a `Try`.
   *
   * This method checks if the input `Try` contains a valid file extension such as "html" or "htm".
   * If the input is a `Success` matching the specified regex without an extension, or with a valid extension,
   * the method returns true. Otherwise, it evaluates the extension and returns false for unsupported types.
   *
   * @param wy a `Try[String]` containing the file name or null to be validated.
   * @return true if the file extension is valid or null; false otherwise.
   */
  private def isValidExtension(wy: Try[String]): Boolean = wy match {
    case Success(fileNameExtensionR(_, _, _, null)) =>
      true
    case Success(fileNameExtensionR(_, _, _, ext)) =>
      ext match {
        case "html" | "htm" =>
          true
        case _ =>
          false
      }
    case _ =>
      true
  }

  /**
   * Determines whether a URL can be read, based on its validity and file extension.
   *
   * @param url the URL to be evaluated
   * @return true if the URL can be read; false if it has an invalid file extension
   */
  private def canReadURL(url: URL): Boolean =
    notNull[URL, String](_.getPath)(url) match {
      case Success(decomposePath(_, ext)) if invalidExt(ext) =>
        false
      case _ =>
        true
    }

  /**
   * Creates a new URL by combining an optional base URL with a resource path, or handles errors if the URL is malformed.
   *
   * This method attempts to construct a valid URL using a base context (if provided) and the specified resource path.
   * If the URL creation fails due to a malformed input, it wraps the exception in a `Failure`.
   * Non-fatal exceptions are also caught, and a custom `WebCrawlerException` is returned as part of the `Failure`.
   *
   * @param context  an optional base URL to resolve the resource against; if None, a standalone URL is created.
   * @param resource the resource path or URL string to be combined with the base context.
   * @return a `Try[URL]` containing the constructed URL if successful, or a `Failure` with an appropriate exception in case of an error.
   */
  private def createRelURL(context: Option[URL], resource: String): Try[URL] =
    Try(new URL(context.orNull, resource)) recoverWith {
      case e: MalformedURLException =>
        Failure(WebCrawlerURLException(context.map(_.toString).getOrElse("") + s"$resource", e))
      case NonFatal(e) =>
        Failure(WebCrawlerException(context.map(_.toString).getOrElse("") + s"$resource", e))
    }

  /**
   * Creates a URL from the given resource string.
   * This method relies on `createRelURL` to construct the URL without a base context.
   *
   * @param resource the resource path or URL string to be converted into a URL.
   * @return a `Try[URL]` containing the constructed URL if successful, or a `Failure` in case of an error.
   */
  def createURL(resource: String): Try[URL] = createRelURL(None, resource)

  /**
   * A function that evaluates a given throwable and determines if it is "forgivable".
   *
   * This function checks the type of the provided throwable against specific exception classes.
   * If the throwable matches either `WebCrawlerProtocolException` or `WebCrawlerFileTypeException`,
   * the function returns true, indicating the exception is forgivable. For all other throwable types,
   * the function returns false.
   *
   * The `exceptionForgivenessFunction` can be utilized in scenarios where certain exceptions, deemed forgivable,
   * should not interrupt the workflow or process execution.
   */
  private lazy val exceptionForgivenessFunction: Throwable => Boolean = {
    case _: WebCrawlerProtocolException =>
      true;
    case _: WebCrawlerFileTypeException =>
      true;
    case _ =>
      false
  }
  /**
   * A value representing a recovery function to handle parse-related exceptions.
   *
   * This function takes an input `Any` and returns a partial function that matches `Throwable` instances.
   * When an exception is encountered, it creates a `Failure` instance with a runtime exception,
   * including details of the input value and the encountered exception.
   *
   * @return A function that, for a given input, produces a partial function handling `Throwable`
   *         and mapping it to a `Failure` of type `Try[Node]`.
   */
  private lazy val parseRecoveryFunction: Any => PartialFunction[Throwable, Try[Node]] = x => {
    case f =>
      Failure(new RuntimeException(s"parse problem with $x: $f"))
  }

  /**
   * Checks if the given file extension is considered invalid.
   *
   * @param ext the file extension to be checked.
   * @return true if the extension is invalid, false otherwise.
   */
  private def invalidExt(ext: String) = List("pptx", "pdf") contains ext

  /**
   * Verifies whether the given protocol is valid by checking it against a predefined list of acceptable protocols.
   *
   * @param protocol the protocol string to validate, such as "http" or "https".
   * @return true if the protocol is valid, false otherwise.
   */
  private def validProtocol(protocol: String) = List("https", "http") contains protocol

  /**
   * Reads the content of the given URL and returns a `BufferedSource` wrapped in a `Try`.
   *
   * @param resource the URL to fetch and read content from
   * @return a `Try` containing the `BufferedSource` if successful, otherwise an exception in case of failure
   */
  private def SourceFromURL(resource: URL): Try[BufferedSource] = Try(Source.fromURL(resource))

  private def urlException[X](w: String): PartialFunction[Throwable, Try[X]] = {
    case NonFatal(e) =>
      Failure(WebCrawlerURLException(w, e))
  }

  private lazy val fileNameExtensionR: Regex = """^([\/\-_~\w]*\/)?([-_\w]*)?(\.(\w*))?$""".r
  private lazy val decomposePath = """(.*)\.(\w+)$""".r
}

/**
 * The `WebCrawler` object implements a basic web crawler to parse and extract URLs from web pages.
 *
 * This object leverages concurrency to crawl multiple URLs simultaneously. It includes functionality
 * to validate, fetch, and parse web page contents, extracting all reachable and parsable links in the process.
 */
@main def runWebCrawler(args: String*): Unit = {

  /**
   * A private instance of `Timer` created using its companion object's `apply` method.
   * This timer is likely used internally for scheduled tasks or timing operations within the enclosing class or object.
   */
  val timer: Timer = Timer.apply

  // Ensure that there's at least one URL to crawl...
  val urls = (args toList).padTo(1, "http://www1.coe.neu.edu/~rhillyard/index.html")
  val result: Try[Seq[URL]] = WebCrawler(5).doMain(urls)
  result match {
    case Success(us) => 
      println(s"WebCrawler: total URLs retrieved: ${us.size}")
      result foreach println
    case Failure(e) => 
      println(s"WebCrawler: error: ${e.getMessage}")
  }
}

/**
 * Custom exception class to represent errors encountered when decoding a URL
 * during the execution of a web crawler.
 *
 * @param url   the URL that caused the exception.
 * @param cause the underlying throwable that caused the exception.
 */
case class WebCrawlerURLException(url: String, cause: Throwable) extends
        Exception(s"Web Crawler could not decode URL: $url", cause)

/**
 * Exception class representing an unsupported protocol encountered during web crawling.
 *
 * This exception is thrown when the web crawler encounters a URL with a protocol
 * that is not supported, such as non-HTTP/HTTPS protocols.
 *
 * @param w the unsupported protocol.
 */
case class WebCrawlerProtocolException(w: String) extends
        Exception(s"Web Crawler does not support protocol: $w")

/**
 * A custom exception class for handling unsupported file types in a web crawler.
 *
 * This exception is thrown when the web crawler encounters a file with an extension
 * that it does not support. The exception includes a message specifying the unsupported
 * file extension.
 *
 * @param w the unsupported file extension that caused this exception.
 */
case class WebCrawlerFileTypeException(w: String) extends
        Exception(s"Web Crawler does not support extension: $w")

/**
 * A custom exception class used specifically for errors occurring within the web crawler application.
 *
 * @param msg   the error message describing the cause of the exception.
 * @param cause the underlying throwable that caused this exception (optional).
 */
case class WebCrawlerException(msg: String, cause: Throwable) extends
        Exception(msg, cause)

/**
 * An object that provides utilities for creating instances of the WebCrawlerException class.
 * This object includes an apply method to simplify exception creation with a message.
 */
object WebCrawlerException {
  def apply(msg: String): WebCrawlerException = apply(msg, null)
}

/**
 * A case class that implements the CharSequence interface, providing custom behaviors
 * for its methods and an additional operation for manipulating strings.
 * An "unstring" is essentially an anti-string which will "eat" the first
 * n characters of any string with which it is concatenated.
 *
 * @constructor Creates an instance of Unstring with a specified integer.
 * @param n an integer parameter used to determine the behavior of the methods.
 */
case class Unstring(n: Int) extends CharSequence {
  /**
   * Concatenates a given string starting from a specified index.
   *
   * @param s the string to be concatenated.
   * @return a new string which is the substring of the input string starting
   *         from the specified index that is determined by the current instance.
   */
  def +(s: String): String = s.substring(n)

  /**
   * Method to return the length of the custom CharSequence.
   *
   * @return the value of -n, where n is the integer parameter of the containing class instance.
   */
  def length(): Int = -n

  /**
   * Returns the character at the specified index of this CharSequence.
   *
   * @param index the index of the character to return. Must be a non-negative integer less than the length of the sequence.
   * @return the character at the specified index of this sequence.
   * @throws UnstringException if the index is invalid or the operation is not supported.
   */
  def charAt(index: Int): Char =
    throw UnstringException(s"charAt: $index")

  /**
   * Returns a subsequence of this character sequence, starting at the specified start index and ending at the specified end index.
   * This method throws an `UnstringException` as it is not supported.
   *
   * @param start the starting index of the subsequence, inclusive.
   * @param end   the ending index of the subsequence, exclusive.
   * @return this method never successfully returns; it always throws an `UnstringException`.
   * @throws UnstringException always thrown when this method is invoked.
   */
  def subSequence(start: Int, end: Int): CharSequence =
    throw UnstringException(s"subSequence: $start, $end")
}

/**
 * Exception thrown when an invalid operation or access is attempted on a custom character sequence class.
 *
 * This exception is used as part of the `Unstring` class to signal errors or unsupported operations
 * such as accessing a character at an invalid index or requesting a subsequence.
 *
 * @param str a string providing details about the specific cause or context of the exception.
 */
case class UnstringException(str: String) extends Exception(str)

@main def junk(): Unit = {

  import scala.concurrent.ExecutionContext.Implicits.global

  val f1 = Future(1)
  val f2 = Future(2)
  val result: Future[Seq[Int]] = Future.sequence(Seq(f1, f2))
  Await.result(result, Duration.Inf).foreach(println)
}