package edu.neu.coe.csye7200.fp

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HTMLTest extends AnyFlatSpec with Matchers {

  import edu.neu.coe.csye7200.fp.HTML.HtmlRules

  val lineSeparator = System.getProperties.getProperty("line.separator")

  behavior of "content"
  it should "work" in {
    HTML("html", "content").content shouldBe "content"
  }

  behavior of "tags"
  it should "work" in {
    HTML("html", Nil, "", Seq(HTML("head"), HTML("body"))).tags shouldBe Seq(HTML("head"), HTML("body"))
  }

  behavior of "name"
  it should "work" in {
    HTML("html").name shouldBe "html"
  }

  behavior of "attributes"
  ignore should "work" in {
    // FIXME
    HTML("x", Map("dir" -> "rtl")).attributes shouldBe Vector("dir" -> "rtl")
  }

  behavior of "apply"
  it should "work for String" in {
    HTML("html") shouldBe HTML("html", Nil, "", Nil)
  }
  it should "work for String, String" in {
    HTML("html", "content") shouldBe HTML("html", Nil, "content", Nil)
  }
  it should "work for String, Seq[String]" in {
    HTML("html", Map("attribute" -> "value")) shouldBe HTML("html", Seq("attribute" -> "value"), "", Nil)
  }
  it should "work for String, Seq[String], String" in {
    HTML("html", Map("attribute" -> "value"), "content") shouldBe HTML("html", Seq("attribute" -> "value"), "content", Nil)
  }

  behavior of "toString"
  it should """match <html></html> for HTML("html")""" in {
    HTML("html").toString shouldBe s"$lineSeparator<html></html>"
  }
  it should """match <html>content</html> for HTML("html","content")""" in {
    HTML("html", "content").toString shouldBe s"$lineSeparator<html>content</html>"
  }
  it should """match <html dir="rtl"></html> for HTML("html",Seq(dir="rtl")""" in {
    HTML("html", Map("dir" -> "rtl")).toString shouldBe
            """
              |<html dir="rtl"></html>""".stripMargin
  }
  it should """match <html><head></head><body></body></html> for HTML("html",Nil,"",Seq(HTML("head"),HTML("body")))""" in {
    HTML("html", Nil, "", Seq(HTML("head"), HTML("body"))).toString shouldBe s"$lineSeparator<html>$lineSeparator<head></head>$lineSeparator<body></body></html>"
  }

  behavior of """\\"""
  it should "work" in {
    HTML("html").\\ shouldBe Seq("html")
    HTML("html", Nil, "", Seq(HTML("head"))).\\ shouldBe Seq("html", "head")
  }

  behavior of "$colon$plus"
  it should "work" in {
    val html0: Tag = HTML("html")
    html0.tags shouldBe Nil
    val html1: Tag = html0 :+ HTML("body")
    html1.tags shouldBe Seq(HTML("body"))
  }

  it should "work for tag tree with :+" in {
    val html0: Tag = HTML("html")
    val html1: Tag = html0 :+ (HTML("header") :+ HTML("title", "my Title")) :+ (HTML("body") :+ HTML("table"))
    html1.\\ shouldBe Seq("html", "header", "title", "body", "table")
  }

  behavior of "unapply"
  ignore should "work" in {
    val html = HTML("html", Seq("attribute" -> "value"), "content", Seq(HTML("head"), HTML("body")))
    // FIXME
    HTML.unapply(html) shouldBe Some("html", Seq("attribute" -> "value"), "content", Seq(HTML("head"), HTML("body")))
  }

}
