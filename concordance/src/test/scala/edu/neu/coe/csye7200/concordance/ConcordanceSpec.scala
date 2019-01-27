package edu.neu.coe.csye7200.concordance

import org.scalatest.{ FlatSpec, Matchers, Inside }

/**
 * @author scalaprof
 * (c) Phasmid Software, 2015
 */
class ConcordanceSpec extends FlatSpec with Matchers with Inside {
  
  def parse(s: String) = {
    val p = new ConcordanceParser
    p.parseAll(p.sentence,s) match {
      case p.Success(ws,_) => ws
      case p.Failure(e,_) => println(e); List()
      case p.Error(e,_) => println(e); List()
    }
  }

  "Concordance" should "read Hello World!" in {
    val r = parse("Hello World!")
    r should matchPattern { case h::tail => }
    r.head should matchPattern { case PositionalString("Hello") => }
    inside(r.head) { case p @ PositionalString(_) =>
        p.pos.line shouldBe (1)
        p.pos.column shouldBe (1)
    }
    r.tail.head should matchPattern { case PositionalString("World!") => }
    inside(r.tail.head) { case p @ PositionalString(_) =>
        p.pos.line shouldBe (1)
        p.pos.column shouldBe (7)
    }
  }
    it should "read Hello<newline>World!" in {
    val r = parse("Hello\nWorld!")
    r should matchPattern { case h::tail => }
    r.head should matchPattern { case PositionalString("Hello") => }
    inside(r.head) { case p @ PositionalString(_) =>
        p.pos.line shouldBe (1)
        p.pos.column shouldBe (1)
    }
    r.tail.head should matchPattern { case PositionalString("World!") => }
    inside(r.tail.head) { case p @ PositionalString(_) =>
        p.pos.line shouldBe (2)
        p.pos.column shouldBe (1)
    }
  }
}
