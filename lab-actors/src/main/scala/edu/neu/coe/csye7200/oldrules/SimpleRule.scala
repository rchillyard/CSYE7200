package edu.neu.coe.csye7200.oldrules

import scala.util._
import scala.util.matching.Regex

/**
  * @author robinhillyard
  */
case class SimpleRule(predicate: Predicate) extends Predicate {
  // TODO don't think we need to override this here
  def apply(candidate: Candidate): Try[Boolean] = predicate(candidate)
}

object SimpleRule {
  // Note that this expression is tail-recursive.
  // That's to say that parentheses can be nested, provided that all adjacent closing parens
  // appear at the termination of the string.
  val rRule: Regex = """^\(([^\)]+)\)\s*(\&|\|)\s*\((.+)\)$""".r
  val pRule: Regex = """^\((.*)\)$""".r

  def apply(s: String): Predicate = s match {
    case rRule(p1, "&", p2) => Predicate(p1) & Predicate(p2)
    case rRule(p1, "|", p2) => Predicate(p1) | Predicate(p2)
    case pRule(p) => Predicate(p)
    case _ => Predicate(s)
  }

}

//case class And(p1: Predicate, p2: Predicate) extends Predicate {
//  def apply(candidate: Candidate): Try[Boolean] = SimpleRule.compose(p1,)
//}
//
//case class OrRule(p1: Predicate, p2: Predicate) extends Predicate {
//  def apply(candidate: Candidate): Either[Throwable, Boolean] = p1(candidate) match {
//    case Right(x) => if (x) Right(true) else p2(candidate)
//    case x => x
//  }
//}
