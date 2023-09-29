package edu.neu.coe.csye7200.assthw

import edu.neu.coe.csye7200.assthw.FizzBuzz.fizzBuzz
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class FizzBuzzSpec extends AnyFlatSpec with should.Matchers {

    behavior of "FizzBuzz"

    it should "fizzBuzz" in {
        fizzBuzz(15) shouldBe "FizzBuzz"
        fizzBuzz(1) shouldBe "1"
        fizzBuzz(3) shouldBe "Fizz"
        fizzBuzz(5) shouldBe "Buzz"
    }

}