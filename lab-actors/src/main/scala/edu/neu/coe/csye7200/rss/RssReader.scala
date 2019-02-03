package edu.neu.coe.csye7200.rss

import java.net.URL
import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import akka.actor.{Actor, ActorSystem, Props}
import akka.event.Logging
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}
import scala.xml.{Elem, NodeSeq, XML}

abstract class Reader extends Actor {
  val log = Logging(context.system, this)

  def print(feed: RssFeed) {
    println(feed.latest)
  }
}

class AtomReader extends Reader {

  val dateFormatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ", Locale.ENGLISH)

  private def parseAtomDate(date: String, formatter: SimpleDateFormat): Date = {
    val newDate = date.reverse.replaceFirst(":", "").reverse
    formatter.parse(newDate)
  }

  private def getHtmlLink(node: NodeSeq) = {
    node
      .filter(n => (n \ "@type").text == "text/html")
      .map(n => (n \ "@href").text).head
  }

  def extract(xml: Elem): Seq[RssFeed] = {
    for (feed <- xml \\ "feed") yield {
      val items = for (item <- feed \\ "entry") yield {
        RssItem(
          (item \\ "title").text,
          getHtmlLink(item \\ "link"),
          (item \\ "summary").text,
          parseAtomDate((item \\ "published").text, dateFormatter),
          (item \\ "id").text)
      }
      AtomRssFeed(
        (feed \ "title").text,
        getHtmlLink(feed \ "link"),
        (feed \ "subtitle ").text,
        items.take(8))
    }
  }

  def receive(): PartialFunction[Any, Unit] = {
    case xml: Elem =>
      extract(xml) match {
        case head :: _ => print(head)
        case Nil =>
      }
  }
}

class XmlReader extends Reader {

  val dateFormatter = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss zzz", Locale.ENGLISH)

  def extract(xml: Elem): Seq[RssFeed] = {

    for (channel <- xml \\ "channel") yield {
      val items = for (item <- channel \\ "item") yield {
        RssItem(
          (item \\ "title").text,
          (item \\ "link").text,
          (item \\ "description").text,
          dateFormatter.parse((item \\ "pubDate").text),
          (item \\ "guid").text)
      }
      XmlRssFeed(
        (channel \ "title").text,
        (channel \ "link").text,
        (channel \ "description").text,
        (channel \ "language").text,
        items.take(8))
    }
  }

  def receive(): PartialFunction[Any, Unit] = {
    case xml: Elem =>
      extract(xml) match {
        case head :: _ => print(head)
        case Nil =>
      }
  }
}

class RssReader extends Actor {
  val log = Logging(context.system, this)

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try {
      op(p)
    } finally {
      p.close()
    }
  }

  def read(url: URL): Unit = {
    Try(url.openConnection.getInputStream) match {
      case Success(u) =>
        val xml = XML.load(u)
        implicit val timeout: Timeout = Timeout(30.seconds)
        val actor = if ((xml \\ "channel").length == 0) context.actorOf(Props[AtomReader])
        else context.actorOf(Props[XmlReader])
        actor ! xml
      case Failure(_) =>
    }
  }

  def receive(): PartialFunction[Any, Unit] = {
    case path: URL => read(path)
  }
}

class SubscriptionReader extends Actor {

  def open(filename: String): Elem = XML.loadFile(filename)

  def read(xml: Elem): Seq[URL] = {
    for {
      node <- xml \\ "@xmlUrl"
    } yield new URL(node.text)
  }

  def receive(): PartialFunction[Any, Unit] = {
    case filename: String =>
      sender ! read(open(filename))
  }
}

object RssReader {

  def getUrls(fileName: String): Array[RssUrl] = getFileLines(fileName).map(url => RssUrl(new URL(url)))

  def getFileLines(fileName: String): Array[String] =
    scala.io.Source.fromFile(fileName).mkString.split("\n").filter(!_.startsWith("#"))

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("RssReader")
    implicit val timeout: Timeout = Timeout(30.seconds)
    val subReader = system.actorOf(Props[SubscriptionReader])
    implicit val dispatcher: ExecutionContextExecutor = system.dispatcher
    for {
      urls <- (subReader ask "subscriptions.xml").mapTo[Seq[URL]]
      url <- urls
    } system.actorOf(Props[RssReader]) ! url

    //system.shutdown()
  }

}
