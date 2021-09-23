package edu.neu.coe.csye7200.fp.enums.caseobject

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * @author scalaprof
  */
//noinspection NameBooleanParameters
class CardsSpec_CaseObject extends AnyFlatSpec with Matchers with Inside {
  "ranks" should "be ordered properly" in {
    assert(Ace > King)
    val ranks = List(Ace, Trey, Four, Queen, Knave, Nine, Seven, Six, Deuce, Five, King, Ten, Eight)
    ranks.sorted(Concept.reverseOrdering) shouldBe List(Ace, King, Queen, Knave, Ten, Nine, Eight, Seven, Six, Five, Four, Trey, Deuce)
  }

  it should "support match" in {
    val x: Rank = Queen
    x match {
      case Queen => assert(true)
      case _ => assert(false)
    }
  }

  it should "distinguish honors" in {
    assert(Ace.isHonor)
    assert(Deuce.isSpot)
  }

  "suits" should "be ordered properly" in {
    val suits = List(Clubs, Hearts, Spades, Diamonds)
    suits.sorted.reverse shouldBe List(Spades, Hearts, Diamonds, Clubs)
  }

  it should "support match" in {
    val x: Suit = Hearts
    x match {
      case Hearts => assert(true)
      case _ => assert(false)
    }
  }

  it should "know the color" in {
    assert(Spades.isBlack)
    assert(Hearts.isRed)
  }

  "cards" should "be ordered properly" in {
    assert(Card(Clubs, Deuce) < Card(Clubs, King))
    val cards = List(Card(Clubs, Deuce), Card(Clubs, King), Card(Clubs, Ten), Card(Spades, Deuce))
    cards.sorted.reverse.head shouldBe Card(Spades, Deuce)
  }

}
