package edu.neu.coe.csye7200.lab99.interviewQuestions

import edu.neu.coe.csye7200.lab99.interviewQuestions.ShuntingYard.parseString
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ShuntingYardSpec extends AnyFlatSpec with should.Matchers {

  behavior of "ShuntingYardSpec"

  it should "parseString" in {
    parseString("1 + 2") shouldBe List(" 2", " ", "2")
  }

}