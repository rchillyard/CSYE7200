package edu.neu.coe.csye7200.lab99.interviewQuestions

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FizzBuzzSpec extends AnyFlatSpec with Matchers {

  behavior of "FizzBuzz"

  it should "work for FizzBuzz" in {
    val fizzBuzz = new FizzBuzz
    fizzBuzz(0) shouldBe "invalid n: 0"
    fizzBuzz(1) shouldBe "1"
    fizzBuzz(2) shouldBe "2"
    fizzBuzz(3) shouldBe "fizz"
    fizzBuzz(5) shouldBe "buzz"
    fizzBuzz(6) shouldBe "fizz"
    fizzBuzz(15) shouldBe "fizzbuzz"
  }

  it should "work for FizzBuzzBrief" in {
    val fizzBuzz = new FizzBuzzBrief
    fizzBuzz(0) shouldBe "invalid n: 0"
    fizzBuzz(1) shouldBe "1"
    fizzBuzz(2) shouldBe "2"
    fizzBuzz(3) shouldBe "fizz"
    fizzBuzz(5) shouldBe "buzz"
    fizzBuzz(6) shouldBe "fizz"
    fizzBuzz(15) shouldBe "fizzbuzz"
  }

  behavior of "Cicadas"
  it should "get Great Eastern brood emergences" in {
    Cicadas.broodX(2021) shouldBe true
    Cicadas.broodX(2022) shouldBe false
  }
  it should "get Great Southern brood emergences" in {
    Cicadas.broodXIX(2024) shouldBe true
    Cicadas.broodX(2023) shouldBe false
  }
}
