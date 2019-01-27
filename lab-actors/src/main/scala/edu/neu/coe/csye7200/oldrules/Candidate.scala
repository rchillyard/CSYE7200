package edu.neu.coe.csye7200.oldrules

/**
  * @author robinhillyard
  */
trait Candidate extends ((String) => Option[Any]) {
  def identifier: String

  def ++(m: Map[String, Any]): Candidate
}

case class MapCandidate(id: String, map: Map[String, Any]) extends Candidate {
  def identifier: String = id

  def ++(m: Map[String, Any]) = MapCandidate(id, map ++ m)

  def apply(s: String): Option[Any] = map.get(s)
}
