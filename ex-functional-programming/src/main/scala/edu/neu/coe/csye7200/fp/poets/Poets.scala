package edu.neu.coe.csye7200.fp.poets

import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import scala.xml.{Elem, Node, NodeSeq, XML}

case class Name(name: String, language: String) {
  def toXML: Elem = <name language={language}>
    {name}
  </name>
}

case class Poet(names: Seq[Name]) {
  def toXML: Elem = <poet>
    {names.map(_.toXML)}
  </poet>
}

object Poet {
  def fromXML(node: Node): Poet = Poet(Name.fromXML(node \ "name"))
}

object Name {
  def getLanguage(x: Option[scala.collection.Seq[Node]]): String = x match {
    case Some(Seq(y)) => y.text
    case _            => ""
  }

  def fromXML(nodes: NodeSeq): Seq[Name] = for {
    node <- nodes
  } yield Name(node.text, getLanguage(node.attribute("language")))
}

object Poets extends App {

  type PoetSeq = Seq[Poet]

  def toXML(poets: PoetSeq) = poets.map(_.toXML)

  val xml = XML.loadFile(getClass.getResource("poets.xml").getPath)
  val poets: PoetSeq = for (poet <- xml \\ "poet") yield Poet.fromXML(poet)
  println(poets)
  println(toXML(poets))

  println("JSON: " + poets.asJson.noSpaces)

  val source = """[{"names":[{"name":"Wang Wei","language":"en"},{"name":"王維","language":"zh"}]},{"names":[{"name":"Li Bai","language":"en"},{"name":"李白","language":"zh"}]}]"""

  decode[PoetSeq](source) match {
    case Right(x) => println(x)
    case Left(e)  => println(s"Parse error: $e")
  }
}