package edu.neu.coe.csye7200

/**
  * Created by scalaprof on 10/31/16.
  */
trait Poet {
  def name: String
}

class TangPoet(val name: String) extends Poet

class SongPoet(val name: String) extends Poet

object TangPoet {
  def apply(name: String): Poet = new TangPoet(name)

  def unapply(user: Poet): Option[String] = Some(user.name)
}

object SongPoet {
  def apply(name: String): Poet = new SongPoet(name)

  def unapply(user: Poet): Option[String] = Some(user.name)
}

object Extractor extends App {
  val poet: Poet = new TangPoet("Li Bai")
  val w1 = poet match {
    case TangPoet(name) => s"Tang poet: $name"
    case _ => "other"
  }
  println(w1)
  val w2 = TangPoet.unapply(poet) match {
    case Some(name) => s"Tang poet: $name"
    case _ => "other"
  }
  println(w2)
}
