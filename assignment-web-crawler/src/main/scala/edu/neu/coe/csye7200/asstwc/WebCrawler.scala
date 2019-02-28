package edu.neu.coe.csye7200.asstwc

import java.net.URL

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.io.Source
import scala.language.postfixOps
import scala.util._
import scala.xml.Node

/**
  * @author scalaprof
  */
object WebCrawler extends App {

  def getURLContent(u: URL): Future[String] = {
    for {
      source <- Future(Source.fromURL(u))
    } yield source mkString
  }

  def wget(u: URL): Future[Seq[URL]] = {
    // Hint: write as a for-comprehension, using the constructor new URL(URL,String) to get the appropriate URL for relative links
    // 16 points.
    def getURLs(ns: Node): Seq[URL] = ??? // TO BE IMPLEMENTED

    def getLinks(g: String): Try[Seq[URL]] =
      for (n <- HTMLParser.parse(g) recoverWith { case f => Failure(new RuntimeException(s"parse problem with URL $u: $f")) })
        yield getURLs(n)
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

  def crawler(depth: Int, args: Seq[URL]): Future[Seq[URL]] = {
    def inner(urls: Seq[URL], depth: Int, accum: Seq[URL]): Future[Seq[URL]] =
      if (depth > 0)
        for (us <- MonadOps.flattenRecover(wget(urls), { x => System.err.println(x) }); r <- inner(us, depth - 1, accum ++: urls)) yield r
      else
        Future.successful(accum)
    inner(args, depth, Nil)
  }

  println(s"web reader: ${args.toList}")
  val urls = for (arg <- args toList) yield Try(new URL(arg))
  val s = MonadOps.sequence(urls)
  s match {
    case Success(z) =>
      println(s"invoking crawler on $z")
      val f = crawler(2, z)
      Await.ready(f, Duration("60 second"))
      for (x <- f) println(s"Links: $x")
    case Failure(z) => println(s"failure: $z")
  }
}
