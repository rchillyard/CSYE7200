package edu.neu.coe.csye7200.fp.enums.caseobject

/**
  * @author robinhillyard
  */
trait Concept extends Ordered[Concept] {
  val name: String
  val priority: Int

  override def toString: String = name

  def compare(that: Concept): Int = priority - that.priority

  def initial: String = name.substring(0, 1)
}

sealed trait Rank extends Concept {
  def isHonor: Boolean = priority > 7

  def isSpot: Boolean = !isHonor
}

sealed trait Suit extends Concept {
  def isRed: Boolean = !isBlack

  def isBlack: Boolean = this match {
    case Spades | Clubs => true
    case _ => false
  }
}

object Concept {
  // This ordering gives the expected rank and suit (in bridge) order, at least for games where A is considered to outrank K.
  implicit def ordering[A <: Concept]: Ordering[A] = Ordering.by(_.priority)

  // This ordering is for the traditional ordering for displaying bridge hands
  def reverseOrdering[A <: Concept]: Ordering[A] = ordering.reverse
}

case object Ace extends Rank {
  val name = "Ace"
  val priority = 12
}

case object King extends Rank {
  val name = "King"
  val priority = 11
}

case object Queen extends Rank {
  val name = "Queen"
  val priority = 10
}

case object Knave extends Rank {
  val name = "Knave"
  val priority = 9

  override def initial = "J"
}

case object Ten extends Rank {
  val name = "10"
  val priority = 8

  override def initial = "T"
}

case object Nine extends Rank {
  val name = "9"
  val priority = 7
}

case object Eight extends Rank {
  val name = "8"
  val priority = 6
}

case object Seven extends Rank {
  val name = "7"
  val priority = 5
}

case object Six extends Rank {
  val name = "6"
  val priority = 4
}

case object Five extends Rank {
  val name = "5"
  val priority = 3
}

case object Four extends Rank {
  val name = "4"
  val priority = 2
}

case object Trey extends Rank {
  val name = "3"
  val priority = 1
}

case object Deuce extends Rank {
  val name = "2"
  val priority = 0
}

case object Spades extends Suit {
  val name = "Spades"
  val priority = 3
}

case object Hearts extends Suit {
  val name = "Hearts"
  val priority = 2
}

case object Diamonds extends Suit {
  val name = "Diamonds"
  val priority = 1
}

case object Clubs extends Suit {
  val name = "Clubs"
  val priority = 0
}

case object Notrump extends Suit {
  val name = "Notrump"
  val priority = 4

  override def initial = "NT"
}

object Rank {
}

object Suit {
}

case class Card(suit: Suit, rank: Rank) extends Ordered[Card] {
  val bridgeStyle = true

  // as opposed to poker-style
  private def nameTuple = (suit.initial, rank.initial)

  override def toString: String = if (bridgeStyle) nameTuple.toString else nameTuple.swap.toString

  def compare(that: Card): Int = suit.compareTo(that.suit) match {
    case 0 => rank.compareTo(that.rank)
    case x => x
  }
}

object Card {
}

object Cards extends App {
  println(List(Card(Clubs, Deuce), Card(Clubs, King), Card(Clubs, Ten), Card(Spades, Deuce)).sorted)
}
