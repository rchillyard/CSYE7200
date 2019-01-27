package edu.neu.coe.csye7200.rss

import java.net.URL
import java.util.Date

import scala.language.postfixOps

case class RssUrl(url: URL) {
  override def toString: String = "RSS: " + url.toString
}

trait RssFeed {
  val link: String
  val title: String
  val desc: String
  val items: Seq[RssItem]

  override def toString: String = title + "\n" + desc + "\n**"

  def latest: RssItem = items sortWith ((a, b) => a.date.compareTo(b.date) > 0) head
}

case class AtomRssFeed(title: String, link: String, desc: String, items: Seq[RssItem]) extends RssFeed

case class XmlRssFeed(title: String, link: String, desc: String, language: String, items: Seq[RssItem]) extends RssFeed

case class RssItem(title: String, link: String, desc: String, date: Date, guid: String) {
  override def toString: String = date + " " + title
}
