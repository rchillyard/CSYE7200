package edu.neu.coe.csye7200.yaml

case class NamedProduct(name: String, map: Map[String,Any]) extends Product {
  override def productElement(n: Int): Any = map.getOrElse(map.keys.toSeq(n), null)

  override def productArity: Int = map.size

  override def productIterator: Iterator[Any] = map.valuesIterator

  override def productPrefix: String = name

  def canEqual(that: Any): Boolean = that match {
    case that: NamedProduct => that.name == name && that.map == map
    case _ => false
  }
}