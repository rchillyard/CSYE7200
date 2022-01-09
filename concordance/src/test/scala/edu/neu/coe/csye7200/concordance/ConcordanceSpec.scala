package edu.neu.coe.csye7200.concordance

import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
/**
  * @author scalaprof
  *         (c) Phasmid Software, 2015
  */
class ConcordanceSpec extends AnyFlatSpec with Matchers with Inside {

  private def parse(s: String) = {
    object CP extends ConcordanceParser
    CP.parseAll(CP.sentence, s) match {
      case CP.Success(ws, _) => ws
      case CP.Failure(e, _) => println(e); List()
      case CP.Error(e, _) => println(e); List()
    }
  }

  "Concordance" should "read Hello World!" in {
    val r = parse("Hello World!")
    r should matchPattern { case _ :: _ => }
    r.head should matchPattern { case PositionalString("Hello") => }
    inside(r.head) { case p@PositionalString(_) =>
      p.pos.line shouldBe 1
      p.pos.column shouldBe 1
    }
    r.tail.head should matchPattern { case PositionalString("World!") => }
    inside(r.tail.head) { case p@PositionalString(_) =>
      p.pos.line shouldBe 1
      p.pos.column shouldBe 7
    }
  }
  it should "read Hello<newline>World!" in {
    val r = parse("Hello\nWorld!")
    r should matchPattern { case _ :: _ => }
    r.head should matchPattern { case PositionalString("Hello") => }
    inside(r.head) { case p@PositionalString(_) =>
      p.pos.line shouldBe 1
      p.pos.column shouldBe 1
    }
    r.tail.head should matchPattern { case PositionalString("World!") => }
    inside(r.tail.head) { case p@PositionalString(_) =>
      p.pos.line shouldBe 2
      p.pos.column shouldBe 1
    }
  }

  it should "doMain" in {
    val concordance = ConcordanceParser.doMain(Array("README.md"))
    concordance.size shouldBe 1
  }
}
