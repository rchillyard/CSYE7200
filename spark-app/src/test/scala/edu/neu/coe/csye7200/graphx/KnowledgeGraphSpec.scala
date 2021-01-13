package edu.neu.coe.csye7200.graphx

import java.time.Instant
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest._

class KnowledgeGraphSpec extends AnyFlatSpec with Matchers {
  "Quantity" should "handle date 2015-11-10T10:15:30.00Z" in {
    val nov10 = Quantity[Instant]("November 10th",Instant.parse("2015-11-10T10:15:30.00Z"))
    nov10.lang shouldBe ("en")
  }
  "NamedEntity" should "person" in {
    val kerry = NamedEntity("Senator John Kerry","John Kerry, U.S. Senator and Secretary of State","person")
    kerry.lang shouldBe ("en")
  }
  "Concept" should "handle invitation to meet" in {
    val meet =  Concept("invitation to meet","meet")
    meet.lang shouldBe ("en")
  }
}
