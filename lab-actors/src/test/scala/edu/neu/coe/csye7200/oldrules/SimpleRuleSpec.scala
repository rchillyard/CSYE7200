package edu.neu.coe.csye7200.oldrules

import org.scalatest.{Inside, Matchers, WordSpecLike}

import scala.util.{Failure, Success}

/**
  * This specification really tests much of the HedgeFund app but because it particularly deals with
  * processing data from the YQL (Yahoo Query Language) using JSON, we call it by its given name.
  */
class SimpleRuleSpec extends WordSpecLike with Matchers with Inside {

  "Simple Predicate and Candidate" in {
    val predicate = NumberPredicate("x", "<", 3)
    val rule = SimpleRule(predicate)
    rule(MapCandidate("test", Map("x" -> "2"))) should matchPattern {
      case Success(true) =>
    }
    rule(MapCandidate("test", Map("x" -> "4"))) should matchPattern {
      case Success(false) =>
    }
  }

  "Simple Predicate, bad Candidate" in {
    val predicate = NumberPredicate("x", "<", 3)
    val rule = SimpleRule(predicate)
    inside(rule(MapCandidate("test", Map("y" -> "2")))) {
      case Failure(x) => println(x)
    }
    inside(rule(MapCandidate("test", Map("x" -> "y")))) {
      case Failure(x) => println(x)
    }
  }

  "Simple Rule" in {
    val predicate = SimpleRule("x < 3")
    predicate should matchPattern {
      case NumberPredicate("x", LessThan(), 3) =>
    }
  }
  "Parenthesized Rule" in {
    val predicate = SimpleRule("(x < 3)")
    predicate should matchPattern {
      case NumberPredicate("x", LessThan(), 3) =>
    }
  }
  "Compound Rule" in {
    val predicate = SimpleRule("(x < 3) & (y > 1)")
    predicate should matchPattern {
      case ComposedPredicate(NumberPredicate("x", LessThan(), 3), NumberPredicate("y", GreaterThan(), 1), Predicate.and) =>
    }
  }

  // FIXME: reimplement rules in terms of Rule class

  //  "Nested Rule" in {
  //    val predicate = SimpleRule("(x < 3) & ((y > 1) | (z = 0))")
  //    predicate should matchPattern {
  //      case
  //      ComposedPredicate(NumberPredicate("x", LessThan(), 3), ComposedPredicate (
  //      NumberPredicate("y", GreaterThan(), 1),
  //      NumberPredicate("z", Equals(), 0), Predicate.or), Predicate.and) =>
  //    }
  //  }
}
