package edu.neu.coe.csye7200.fp.sorting

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CardSpec extends AnyFlatSpec with Matchers {

  behavior of "rank"

  it should "use implicit conversion" in {
    val rank2: Rank = "2"
    rank2 shouldBe Deuce
    val rankA: Rank = "A"
    rankA shouldBe Ace
  }

  it should "parse string" in {
    Rank("2") shouldBe Deuce
    Rank("T") shouldBe Ten
    Rank("10") shouldBe Ten
  }

  it should "not parse string" in {
    an[CardException] should be thrownBy Rank("Z")
  }

  it should "form string" in {
    Deuce.toString shouldBe "2"
    Ace.toString shouldBe "A"
  }

  it should "create spot rank from Int" in {
    Rank(2) shouldBe Deuce
    Rank(10) shouldBe Ten
    Rank(14) shouldBe Ace
  }

  it should "implement compare" in {
    implicitly[Ordered[Rank]](Deuce).compare(Trey) shouldBe -1
  }

  it should "sort in order" in {
    val target: List[Rank] = List(Deuce, Ace, Ten)
    target.sorted.reverse shouldBe List(Ace, Ten, Deuce)
  }

  it should "implement isHonor" in {
    Deuce.isHonor shouldBe false
    Ace.isHonor shouldBe true
  }

  it should "implement spot" in {
    Spot(9) shouldBe Nine
    Spot(10) shouldBe Ten
    Spot(11) shouldBe Jack
  }

  behavior of "suit"

  it should "use implicit conversion" in {
    val suit: Suit = "S"
    suit shouldBe Spades
  }

  it should "form string" in {
    Spades.toString shouldBe "S"
    Clubs.toString shouldBe "C"
  }

  it should "implement compare" in {
    implicitly[Ordered[Suit]](Clubs).compare(Spades) shouldBe -3
  }

  it should "parse string" in {
    Suit('S') shouldBe Spades
  }

  it should "not parse string" in {
    an[CardException] should be thrownBy Suit('X')
  }

  it should "sort in order" in {
    val target: List[Suit] = List(Diamonds, Hearts, Spades, Clubs)
    target.sorted.reverse shouldBe List(Spades, Hearts, Diamonds, Clubs)
  }

  it should "implement priority" in {
    Clubs.priority shouldBe 3
    Spades.priority shouldBe 0
    Hearts.priority shouldBe 1
    Diamonds.priority shouldBe 2
  }

  it should "implement isRound" in {
    Clubs.isRound shouldBe true
    Spades.isRound shouldBe false
    Hearts.isRound shouldBe true
    Diamonds.isRound shouldBe false
  }

  it should "implement isRed" in {
    Clubs.isRed shouldBe false
    Spades.isRed shouldBe false
    Hearts.isRed shouldBe true
    Diamonds.isRed shouldBe true
  }

  behavior of "card"

  it should "use implicit conversion" in {
    val card: Card = "S2"
    card shouldBe Card(Spades, Rank("2"))
  }

  it should "form string" in {
    Card("SQ").toString shouldBe "SQ"
  }

  it should "parse string" in {
    Card("S2") shouldBe Card(Spades, Rank("2"))
  }

  it should "not parse string" in {
    an[CardException] should be thrownBy Card("X0")
  }

  it should "implement compare" in {
    implicitly[Ordered[Card]](Card(Spades, "A")).compare(Card(Spades, "K")) shouldBe 1
  }

  it should "sort in proper order" in {
    val target: List[Card] = List("SA", "DT", "S2", "SK")
    target.sorted.reverse shouldBe List[Card]("SA", "SK", "S2", "DT")
  }

  behavior of "holding"

  it should "form string" in {
    Holding(Spades, "2", "A").toString shouldBe "S2A"
    Holding(Spades).toString shouldBe "S-"
    import Rank._
    Holding.create(Spades, Seq[Rank]("2", "A")).toString shouldBe "SA2"
  }

  behavior of "hand"

  it should "form string" in {
    val target = Hand("SAT32", "CQT98", "D43", "HKJT")
    target.toString shouldBe "SAT32 CQT98 D43 HKJT"
  }


}
