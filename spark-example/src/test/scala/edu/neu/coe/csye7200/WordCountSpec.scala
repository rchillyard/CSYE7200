package edu.neu.coe.csye7200

import org.scalatest.{FlatSpec, Matchers}

class WordCountSpec extends FlatSpec with Matchers {

  behavior of "myFilter"

  it should "work" in {
    WordCount.myFilter("Hello","He") shouldBe false
    WordCount.myFilter("Hello","he") shouldBe true
  }

  behavior of "myReplace"

  it should "work" in {
    WordCount.myReplacer("abc,") shouldBe "abc"
    WordCount.myReplacer("abc") shouldBe "abc"
  }

}
