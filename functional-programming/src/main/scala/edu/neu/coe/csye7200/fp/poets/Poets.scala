package edu.neu.coe.csye7200.fp.poets

import scala.language.postfixOps
import scala.xml.{Elem, Node, NodeSeq, XML}

case class Name(name: String, language: String) {
  def toXML: Elem = <name language={language}>{name}</name>
}

case class Poet(names: Seq[Name]) {
  def toXML: Elem = <poet>{names map (_.toXML)}</poet>
}

object Poet {
  def fromXML(node: Node) = Poet(Name.fromXML(node \ "name"))
}

object Name {
  def getLanguage(x: Option[scala.collection.Seq[Node]]): String = x match {
    case Some(Seq(y)) => y.text;
    case _ => ""
  }

  def fromXML(nodes: NodeSeq): Seq[Name] = for {
    node <- nodes
  } yield Name(node.text, getLanguage(node.attribute("language")))
}

/**
  * @author scalaprof
  */
object Poets extends App {

  import spray.json._

  type PoetSeq = Seq[Poet]

  def toXML(poets: PoetSeq) = poets map (_ toXML)

  val xml = XML.loadFile(getClass.getResource("poets.xml").getPath)
  val poets: PoetSeq = for (poet <- xml \\ "poet") yield Poet.fromXML(poet)
  println(poets)
  println(toXML(poets))

  case class Poets(poets: PoetSeq)

  object PoetsJsonProtocol extends DefaultJsonProtocol {
    implicit val nameFormat: RootJsonFormat[Name] = jsonFormat2(Name.apply)
    implicit val poetFormat: RootJsonFormat[Poet] = jsonFormat1(Poet.apply)
    implicit val poetsFormat: RootJsonFormat[Poets] = jsonFormat1(Poets)
  }

  import PoetsJsonProtocol._

  println("JSON: " + poets.toJson)

  private def fromJson(string: String) = string.parseJson.convertTo[PoetSeq]

  val source = """[{"names":[{"name":"Wang Wei","language":"en"},{"name":"王維","language":"zh"}]},{"names":[{"name":"Li Bai","language":"en"},{"name":"李白","language":"zh"}]}]"""

  val x = fromJson(source)

  println(x)
}
