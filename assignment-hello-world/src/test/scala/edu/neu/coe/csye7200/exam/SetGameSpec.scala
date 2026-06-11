package edu.neu.coe.csye7200.exam

import edu.neu.coe.csye7200.exam.Color.Red
import edu.neu.coe.csye7200.exam.Count.{One, Three, Two}
import edu.neu.coe.csye7200.exam.Shading.{Open, Solid, Striped}
import edu.neu.coe.csye7200.exam.Shape.{Diamond, Oval, Squiggle}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.Random

class SetGameSpec extends AnyFlatSpec with should.Matchers {

  behavior of "SetGame"

  val game = SetGame(12, new Random(0L))
  val card1DRO = Card(One, Diamond, Red, Open)
  val card2DRO = Card(Two, Diamond, Red, Open)
  val card3ORD = Card(Three, Oval, Red, Striped)
  val card1SRS = Card(One, Squiggle, Red, Solid)

  it should "sets" in {
    game.sets(game.initialHand).distinct.size shouldBe 3
  }

  it should "allCards" in {
    game.allCards.size shouldBe 81
    game.allCards.distinct.size shouldBe 81
  }

  it should "hand" in {
    game.hand.size shouldBe 12
    game.initialHand.distinct.size shouldBe 12
  }

  it should "doMain" in {
    import java.io.{PrintWriter, StringWriter}

    val sw = new StringWriter()
    val pw: PrintWriter = new PrintWriter(sw, true)
    doMain(game, pw)
    pw.flush()
    pw.close()
    sw.close()
    val expected =
      """There are 81 cards in total.
        |Example hand: Two:Squiggle:Green:Striped, Three:Squiggle:Green:Open, One:Diamond:Green:Striped, Three:Squiggle:Green:Solid, Two:Squiggle:Purple:Open, Three:Oval:Purple:Striped, Two:Diamond:Green:Striped, Two:Diamond:Red:Solid, Two:Oval:Green:Open, One:Oval:Red:Open, Three:Diamond:Green:Striped, Three:Oval:Red:Solid
        |There are 3 possible sets: {One:Diamond:Green:Striped, Three:Squiggle:Green:Solid, Two:Oval:Green:Open}, {One:Diamond:Green:Striped, Two:Squiggle:Purple:Open, Three:Oval:Red:Solid}, {One:Diamond:Green:Striped, Two:Diamond:Green:Striped, Three:Diamond:Green:Striped}
        |""".stripMargin
    sw.toString shouldBe expected
  }

  behavior of "Set"

  it should "match first set" in {
    val sets = game.sets(game.initialHand)
    sets.head shouldBe Set(Card("One:Diamond:Green:Striped"), Card("Three:Squiggle:Green:Solid"), Card("Two:Oval:Green:Open"))
  }

  it should "toString" in {
    Set(card2DRO, card3ORD, card1SRS).toString shouldBe "{Two:Diamond:Red:Open, Three:Oval:Red:Striped, One:Squiggle:Red:Solid}"
  }

  it should "apply(String)" in {
    Set("{Two:Diamond:Red:Open, Three:Oval:Red:Striped, One:Squiggle:Red:Solid}") shouldBe Set(card2DRO, card3ORD, card1SRS)
  }

  it should "isSet" in {
    Set(card2DRO, card3ORD, card1SRS).isSet shouldBe true
    Set(card1DRO, card3ORD, card1SRS).isSet shouldBe false
  }
  it should "invalid Set" in {
    an[IllegalArgumentException] shouldBe thrownBy(Set(Seq()))
    an[IllegalArgumentException] shouldBe thrownBy(Set(card1DRO, card1DRO, card1DRO))
    an[IllegalArgumentException] shouldBe thrownBy(Set(card1DRO, Card(One, Diamond, Red, Open), card1DRO))
  }

  behavior of "Card"

  it should "toString" in {
    Card(Two, Diamond, Red, Open).toString shouldBe "Two:Diamond:Red:Open"
    Card(Three, Oval, Red, Striped).toString shouldBe "Three:Oval:Red:Striped"
    Card(One, Squiggle, Red, Solid).toString shouldBe "One:Squiggle:Red:Solid"
  }

  it should "apply(String)" in {
    Card("Two:Diamond:Red:Open") shouldBe Card(Two, Diamond, Red, Open)
    Card("Three:Oval:Red:Striped") shouldBe Card(Three, Oval, Red, Striped)
    Card("One:Squiggle:Red:Solid") shouldBe Card(One, Squiggle, Red, Solid)
  }
}
