package edu.neu.coe.csye7200.fp.enums.enumeration

import scala.language.implicitConversions

/**
  * @author scalaprof
  */

object Rank extends Enumeration {
  type Rank = Value
  val Deuce, Trey, Four, Five, Six, Seven, Eight, Nine, Ten, Knave, Queen, King, Ace = Value

  class RankValue(rank: Value) {
    override def toString: String = {
      val s = super.toString
      if (isHonor) s else s.toLowerCase
    }

    def isSpot: Boolean = !isHonor

    def isHonor: Boolean = rank match {
      case Ace | King | Queen | Knave | Ten => true
      case _ => false
    }
  }

  implicit def value2RankValue(rank: Value): RankValue = new RankValue(rank)
}

import scala.language.implicitConversions

object Suit extends Enumeration {
  type Suit = Value
  val Clubs, Diamonds, Hearts, Spades = Value

  class SuitValue(suit: Value) {
    def isRed: Boolean = !isBlack

    def isBlack: Boolean = suit match {
      case Clubs | Spades => true
      case _ => false
    }
  }

  implicit def value2SuitValue(suit: Value): SuitValue = new SuitValue(suit)
}

import edu.neu.coe.csye7200.fp.enums.enumeration.Rank._
import edu.neu.coe.csye7200.fp.enums.enumeration.Suit._


case class Card(rank: Rank, suit: Suit)
