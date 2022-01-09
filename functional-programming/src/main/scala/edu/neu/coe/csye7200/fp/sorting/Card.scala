package edu.neu.coe.csye7200.fp.sorting

import scala.language.implicitConversions

/**
  * This module is designed primarily to demonstrate the use of implicits and, especially, Ordering.
  */


/**
  * This class models a playing card.
  *
  * @param suit the suit of the card (spades, hearts, diamonds, or clubs).
  * @param rank the rank of the card (2 thru A).
  */
case class Card(suit: Suit, rank: Rank) {
  override def toString: String = s"$suit$rank" // Bridge order (not Poker)
}

/**
  * This class models a holding in a suit.
  *
  * @param suit  the suit.
  * @param ranks the ranks (expected to be in order).
  */
case class Holding(suit: Suit, ranks: Rank*) {
  override def toString: String = s"$suit${Holding.ranksToString(ranks)}"
}

/**
  * This class models a bridge hand (four suits).
  *
  * @param holdings the four holdings (as a Map)
  */
case class Hand(holdings: Map[Suit, Holding]) {
  override def toString: String = s"""${holdings.values.mkString("", " ", "")}"""

}

/**
  * Trait defining the properties of a suit
  */
trait Suit {
  val isRound: Boolean
  val isRed: Boolean
}

/**
  * Trait defining the properties of a rank
  */
trait Rank extends Priority {
  val isHonor: Boolean
}

trait Priority {
  def priority: Int
}

/**
  * Abstract base class for Suit.
  *
  * @param isRound true if this is hearts or clubs.
  * @param isRed   true if this is hearts or diamonds.
  */
abstract class BaseSuit(val isRound: Boolean, val isRed: Boolean) extends Suit with Priority {
  def priority: Int = Card.bool2Int(isRound) + 2 * Card.bool2Int(isRound ^ isRed)

  override def toString: String = List("S", "H", "D", "C")(priority)
}

case object Spades extends BaseSuit(false, false)

case object Hearts extends BaseSuit(true, true)

case object Diamonds extends BaseSuit(false, true)

case object Clubs extends BaseSuit(true, false)

/**
  * Companion object for Suit.
  */
object Suit {
  /**
    * Implicitly convert String to Suit.
    *
    * @param s the String.
    * @return the Rank.
    */
  implicit def convertStringToSuit(s: String): Suit = apply(s.head)

  /**
    * Explicitly convert a Char into a Suit.
    *
    * @param c the Char.
    * @return the Suit.
    */
  def apply(c: Char): Suit = c match {
    case 'S' | 's' => Spades
    case 'H' | 'h' => Hearts
    case 'D' | 'd' => Diamonds
    case 'C' | 'c' => Clubs
    case _ => throw CardException(s"$c is not a suit")
  }

  /**
    * Define an ordering for Suits.
    */
  implicit object SuitOrdering extends Ordering[Suit] {
    override def compare(x: Suit, y: Suit): Int = -x.asInstanceOf[Priority].priority + y.asInstanceOf[Priority].priority
  }

}

/**
  * Abstract base class for Rank.
  *
  * @param priority the priority of this Rank.
  * @param isHonor  true if this Rank is an honor.
  */
abstract class BaseRank(val priority: Int, val isHonor: Boolean) extends Rank with Priority {

  override def toString: String = if (isHonor) List("A", "K", "Q", "J", "T")(priority) else (14 - priority).toString

  private def canEqual(other: Any): Boolean = other.isInstanceOf[BaseRank]

  override def equals(other: Any): Boolean = other match {
    case that: BaseRank =>
      (that canEqual this) &&
              priority == that.priority
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(priority)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

/**
  * Companion object for Rank
  */
object Rank {

  /**
    * Implicit converter from String to Rank
    *
    * @param s the String.
    * @return the Rank.
    */
  implicit def convertStringToRank(s: String): Rank = apply(s)

  /**
    * Defines an ordering of Ranks
    */
  implicit object RankOrdering extends Ordering[Rank] {
    override def compare(x: Rank, y: Rank): Int = -x.priority + y.priority
  }

  /**
    * Method to create a Rank from an honor String (A, K, Q, J, or T)
    *
    * @param w the String
    * @return the Rank.
    */
  def honor(w: String): Rank = w match {
    case "A" => Ace
    case "K" => King
    case "Q" => Queen
    case "J" => Jack
    case "T" => Ten
    case _ => throw CardException(s"$w is not an honor rank")
  }

  /**
    * Apply method for rank r.
    * NOTE: this also works for honor ranks.
    *
    * @param r the number of spots on the card.
    * @return the Rank.
    */
  def apply(r: Int): Rank = Spot(r)

  /**
    * Method to (explicitly) create a Rank from a String
    *
    * @param s the String.
    * @return the Rank.
    */
  def apply(s: String): Rank = s match {
    case spotR(n) => Spot(n.toInt)
    case honorR(w) => honor(w)
    case _ => throw CardException(s"$s is not a rank")
  }

  private val spotR = """(\d\d?)""".r
  private val honorR = """([AKQJT])""".r
}

/**
  * Case class corresponding to a spot Rank.
  *
  * @param spot the spot value.
  */
case class Spot(spot: Int) extends BaseRank(14 - spot, spot > 9)

case object Deuce extends BaseRank(12, false)

case object Trey extends BaseRank(11, false)

case object Four extends BaseRank(10, false)

case object Five extends BaseRank(9, false)

case object Six extends BaseRank(8, false)

case object Seven extends BaseRank(7, false)

case object Eight extends BaseRank(6, false)

case object Nine extends BaseRank(5, false)

case object Ten extends BaseRank(4, true)

case object Jack extends BaseRank(3, true)

case object Queen extends BaseRank(2, true)

case object King extends BaseRank(1, true)

case object Ace extends BaseRank(0, true)

/**
  * Companion object for Holding.
  */
object Holding {

  implicit def convertStringToHolding(s: String): Holding = create(Suit(s.head), Card.parser.parseRanks(s.tail))

  def create(suit: Suit, ranks: Seq[Rank]): Holding = Holding(suit, ranks.sorted.reverse: _*)

  def ranksToString(ranks: Seq[Rank]): String = if (ranks.nonEmpty) ranks.mkString("", "", "") else "-"
}

object Hand {
  def apply(ws: String*): Hand = {
    val tuples = for (w <- ws; h = Holding.convertStringToHolding(w)) yield h.suit -> h
    Hand(tuples.toMap)
  }
}

/**
  * Companion object for Card.
  */
object Card {

  /**
    * Explicit conversion of String to Card
    *
    * @param s the String.
    * @return the Card.
    */
  def apply(s: String): Card = {
    val suit = s.head match {
      case 'S' | 's' => Spades
      case 'H' | 'h' => Hearts
      case 'D' | 'd' => Diamonds
      case 'C' | 'c' => Clubs
      case c => throw CardException(s"$c is not a suit symbol")
    }
    val rank = Rank(s.tail)
    Card(suit, rank)
  }

  /**
    * Implicit conversion of String to Card.
    *
    * @param s the String.
    * @return the Card.
    */
  implicit def convertStringToCard(s: String): Card = apply(s)

  /**
    * Defines an ordering of Ranks
    */
  implicit object CardOrdering extends Ordering[Card] {
    override def compare(x: Card, y: Card): Int = {
      import Rank._
      import Suit._
      val cf = SuitOrdering.compare(x.suit, y.suit)
      if (cf != 0) cf
      else RankOrdering.compare(x.rank, y.rank)
    }
  }

  private[sorting] def bool2Int(b: Boolean): Int = if (b) 1 else 0

  private[sorting] val parser = RankParser
}

/**
  * Exception defined for this module.
  *
  * @param w the message.
  */
case class CardException(w: String) extends Exception(w)

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Parser of rank strings.
  */
object RankParser extends JavaTokenParsers {

  /**
    * Method to parse ranks as a sequence of Rank
    *
    * @param w the String to parse
    * @return a Seq[Rank]
    */
  def parseRanks(w: String): Seq[Rank] = {
    val parsed: ParseResult[Seq[Rank]] = parseAll(holding, w)
    parsed match {
      case Success(rs, _) => rs
      case Failure(x, input) => throw CardException(x + ": " + input)
      case Error(_, _) => throw CardException("error")
    }
  }

  def holding: Parser[List[Rank]] = rep(rank) ^^ (_ map Rank.apply)

  def rank: Parser[String] = """[2-9]""".r | """[AKQJT]""".r | "10" | failure("invalid rank")
}
