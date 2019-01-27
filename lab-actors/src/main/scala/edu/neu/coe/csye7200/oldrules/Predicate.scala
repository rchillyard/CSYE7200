package edu.neu.coe.csye7200.oldrules

import scala.util._

/**
  * @author robinhillyard
  */
trait Predicate extends ((Candidate) => Try[Boolean]) {
  def &(p: Predicate): Predicate = ComposedPredicate(this, p, Predicate.and)

  def |(p: Predicate): Predicate = ComposedPredicate(this, p, Predicate.or)
}

object Predicate {
  def apply(predicate: String): Predicate = {
    val rNumPredicate = """^\s*(\w+)\s*([=<>]{1,2})\s*(-?[0-9]+\.?[0-9]*)\s*$""".r
    val rTextPredicate = """^\s*(\w+)\s*([=<>]{1,2})\s*(\w+)\s*$""".r
    predicate match {
      case "Always" => Always
      case "Never" => Never
      case rNumPredicate(v, o, n) => NumberPredicate(v, o, n)
      case rTextPredicate(v, o, n) => StringPredicate(v, o, n)
      case _ => throw new Exception(s"predicate: $predicate is malformed")
    }
  }

  def map2(xy: Try[Boolean], yy: => Try[Boolean])(f: (Boolean, Boolean) => Boolean): Try[Boolean] = for (x <- xy; y <- yy) yield f(x, y)

  //  def compose(p1: Predicate, p2: Predicate)(f: (Boolean,Boolean)=>Boolean): Predicate = new Predicate {
  //    def apply(c: Candidate): Try[Boolean] = map2(p1(c), p2(c))(f)
  //  }

  val and: (Boolean, Boolean) => Boolean = _ && _
  val or: (Boolean, Boolean) => Boolean = _ || _
  //  def and(p1: Predicate, p2: Predicate): Predicate = compose(p1,p2)(_ && _)
  //  def or(p1: Predicate, p2: Predicate): Predicate = compose(p1,p2)(_ || _)

}

case class ComposedPredicate(p1: Predicate, p2: Predicate, f: (Boolean, Boolean) => Boolean) extends Predicate {
  def apply(c: Candidate): Try[Boolean] = Predicate.map2(p1(c), p2(c))(f)
}


case object Always extends Predicate {
  def apply(candidate: Candidate) = Success(true)
}

case object Never extends Predicate {
  def apply(candidate: Candidate) = Success(false)
}
