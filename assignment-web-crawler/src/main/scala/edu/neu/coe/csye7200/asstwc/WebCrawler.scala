package edu.neu.coe.csye7200.asstwc

import java.net.URL
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.io.{BufferedSource, Source}
import scala.language.postfixOps
import scala.util._
import scala.util.control.NonFatal
import scala.xml.Node

/**
  * @author scalaprof
  */
object WebCrawler extends App {

  def getURLContent(u: URL): Future[String] =
    for {
      s <- MonadOps.asFuture(SourceFromURL(u))
      w <- MonadOps.asFuture(sourceToString(s, s"Cannot read from source at $u"))
    } yield w

  def wget(u: URL): Future[Seq[URL]] = {
    // Hint: write as a for-comprehension, using the method createURL(Option[URL], String) to get the appropriate URL for relative links
    // 16 points.
    def getURLs(ns: Node): Seq[Try[URL]] = ??? // TO BE IMPLEMENTED

    def getLinks(g: String): Try[Seq[URL]] = {
      val ny = HTMLParser.parse(g) recoverWith { case f => Failure(new RuntimeException(s"parse problem with URL $u: $f")) }
      for (n <- ny; z <- MonadOps.sequence(getURLs(n))) yield z
    }
    // Hint: write as a for-comprehension, using getURLContent (above) and getLinks above. You might also need MonadOps.asFuture
    // 9 points.
    ??? // TO BE IMPLEMENTED
  }

  def wget(us: Seq[URL]): Future[Seq[Either[Throwable, Seq[URL]]]] = {
    val us2 = us.distinct take 10
    // Hint: Use wget(URL) (above). MonadOps.sequence and Future.sequence are also available to you to use.
    // 15 points. Implement the rest of this, based on us2 instead of us.
    // TO BE IMPLEMENTED
    ???
  }

  def crawler(depth: Int, us: Seq[URL]): Future[Seq[URL]] = {
    def inner(urls: Seq[URL], depth: Int, accum: Seq[URL]): Future[Seq[URL]] =
      if (depth > 0)
        for (us <- MonadOps.flattenRecover(wget(urls), { x => System.err.println(s"""crawler: ignoring exception $x ${if (x.getCause != null) " with cause " + x.getCause else ""}""") }); r <- inner(us, depth - 1, accum ++: urls)) yield r
      else
        Future.successful(accum)

    inner(us, depth, Nil)
  }

  println(s"web reader: ${args.toList}")
  val uys = for (arg <- args toList) yield getURL(arg)
  val s = MonadOps.sequence(uys)
  s match {
    case Success(z) =>
      println(s"invoking crawler on $z")
      val f = crawler(2, z)
      Await.ready(f, Duration("60 second"))
      for (x <- f) println(s"Links: $x")
    case Failure(z) => println(s"failure: $z")
  }

  private def sourceToString(source: BufferedSource, errorMsg: String): Try[String] =
    try Success(source mkString) catch {
      case NonFatal(e) => Failure(WebCrawlerURLException(errorMsg, e))
    }

  private def getURL(resource: String): Try[URL] = createURL(null, resource)

  private def createURL(context: Option[URL], resource: String): Try[URL] =
    try Success(new URL(context.orNull, resource)) catch {
      case NonFatal(e) =>
        val message: String = s"""Bad URL: ${if (context.isDefined) "context: " + context else ""} resource=$resource"""
        Failure(WebCrawlerURLException(message, e))
    }

  private def SourceFromURL(resource: URL): Try[BufferedSource] =
    try Success(Source.fromURL(resource)) catch {
      case NonFatal(e) => Failure(WebCrawlerURLException(s"""Cannot get source from URL: $resource""", e))
    }
}

case class WebCrawlerURLException(url: String, cause: Throwable) extends Exception(s"Web Crawler could not decode URL: $url", cause)

case class WebCrawlerException(msg: String, cause: Throwable) extends Exception(msg, cause)

object WebCrawlerException {
  def apply(msg: String): WebCrawlerException = apply(msg, null)
}
