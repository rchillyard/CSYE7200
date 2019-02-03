package edu.neu.coe.csye7200.oldrules

import org.scalatest.{Inside, Matchers, WordSpecLike}

import scala.util.{Failure, Success}

/**
  * This specification really tests much of the HedgeFund app but because it particularly deals with
  * processing data from the YQL (Yahoo Query Language) using JSON, we call it by its given name.
  */
class PredicateSpec extends WordSpecLike with Matchers with Inside {

  "Simple Predicate and Candidate" in {
    val predicate = NumberPredicate("x", "<", 3)
    predicate(MapCandidate("test", Map("x" -> "2"))) should matchPattern {
      case Success(true) =>
    }
    predicate(MapCandidate("test", Map("x" -> "4"))) should matchPattern {
      case Success(false) =>
    }
  }

  "Simple Predicate, bad Candidate" in {
    val predicate = NumberPredicate("x", "<", 3)
    inside(predicate(MapCandidate("test", Map("y" -> "2")))) {
      case Failure(x) => println(x)
    }
    inside(predicate(MapCandidate("test", Map("x" -> "y")))) {
      case Failure(x) => println(x)
    }
  }

  "String Predicate" in {
    val predicate = Predicate("x < 3")
    predicate should matchPattern {
      case NumberPredicate("x", LessThan(), 3) =>
    }
    predicate shouldEqual NumberPredicate("x", "<", 3)
  }

  "Text Predicate" in {
    val predicate = Predicate("x == Hello")
    predicate(MapCandidate("test", Map("x" -> "Hello"))) should matchPattern {
      case Success(true) =>
    }
  }
}
