package edu.neu.coe.csye7200.fp

import com.phasmidsoftware.parse.CellParser
import scala.language.implicitConversions

case class Attribute(key: String, value: String) {
  override def toString: String = s"""$key="$value""""
}

/**
  * Trait Tag to model an Markup Language-type document.
  */
trait Tag {

  /**
    * Method to yield the name of this Tag
    *
    * @return the name, that's to say what goes between &lt; and &gt;
    */
  def name: String

  /**
    * Method to yield the attributes of this Tag.
    *
    * @return a sequence of Attributes
    */
  def attributes: Seq[Attribute]

  /**
    * Method to yield the content of this Tag.
    *
    * @return the content as a String.
    */
  def content: String

  /**
    * Method to yield the child Tags of this Tag.
    *
    * @return a Seq of Tags.
    */
  def tags: Seq[Tag]

  /**
    * Method to add a child to this Tag
    *
    * @param tag the tag to be added
    * @return a new version of this Tag with the additional tag added as a child
    */
  def :+(tag: Tag): Tag

  /**
    * Method to yield the tag names depth-first in a Seq
    *
    * @return a sequence of tag names
    */
  def \\ : Seq[String] = name +: (for (t <- tags; x <- t.\\) yield x)
}
abstract class BaseTag(name: String, attributes: Seq[Attribute], content: String, tags: Seq[Tag])(implicit rules: TagRules) extends Tag {

  override def toString: String = s"""$lineSeparator${tagString()}$content$tagsString${tagString(true)}"""

  private def attributeString(close: Boolean) = if (close || attributes.isEmpty) "" else " " + attributes.mkString(" ")

  private def tagsString = if (tags.isEmpty) "" else tags mkString ""

  private def nameString(close: Boolean = false) = (if (close) "/" else "") + name

  private def tagString(close: Boolean = false) = s"<${nameString(close)}${attributeString(close)}>"

  val lineSeparator = System.getProperties.getProperty("line.separator")
}

/**
  * Case class to model an HTML document.
  *
  * @param name       the name of the tag at the root of the document.
  * @param attributes the attributes of the tag.
  * @param content    the content of the tag.
  * @param tags       the child tags.
  * @param rules      the "rules" (currently ignored) but useful in the future to validate documents.
  */
case class HTML(name: String, attributes: Seq[Attribute], content: String, tags: Seq[Tag])(implicit rules: TagRules) extends BaseTag(name, attributes, content, tags) {

  /**
    * Method to add a child to this Tag
    *
    * @param tag the tag to be added
    * @return a new version of this Tag with the additional tag added as a child
    */
  override def :+(tag: Tag): Tag = HTML(name, attributes, content, tags :+ tag)
}

trait HTMLRenderable[T] {
  def toHTML(t: T)(implicit rules: TagRules): HTML
}

// TODO what's this all about?
trait HTMLRenderableProduct[T <: Product] {
  def toHTML[U <: T : CellParser](t: T)(implicit rules: TagRules): HTML = {
    val cp = implicitly[CellParser[U]]

//    val x: Method = implicitly[ClassTag[T]].runtimeClass.getDeclaredMethod("unapply", classOf[Any])
//    val z: AnyRef = x.invoke(t)
    HTML("bad")
  }
}

trait HTMLRenderableOption[T] {
  def toHTML(to: Option[T])(implicit rules: TagRules, hr: HTMLRenderable[T]): HTML = to match {
    case Some(t) => implicitly[HTMLRenderable[T]].toHTML(t)
    case None => implicitly[HTMLRenderable[T]].toHTML(default)
  }

  def default: T
}

object Attribute {
  def apply(kv: (String, String)): Attribute = convertFromTuple(kv)

  def mapToAttributes(m: Map[String, String]): Seq[Attribute] = m.toSeq.map(apply)

  implicit def convertFromTuple(kv: (String, String)): Attribute = Attribute(kv._1, kv._2)
}

/**
  * Companion object to HTML
  */
object HTML {

  implicit object HtmlRules extends TagRules

  def apply(name: String, attributes: Map[String, String], content: String): HTML = apply(defaultName(name), for (attr <- attributes.toSeq) yield Attribute(attr), content, Nil)

  def apply(name: String, attributes: Map[String, String]): HTML = apply(name, attributes, "")

  def apply(name: String): HTML = apply(name, Map.empty, "")

  def apply(name: String, content: String): HTML = apply(name, Map.empty, content)

  def defaultName(name: String): String = if (name == "") "span" else name
}

/**
  * For future expansion.
  * The tag rules will allow us to check the model of a Tag.
  * For example, does it conform to HTML5?
  * Or XML, etc?
  */
trait TagRules
