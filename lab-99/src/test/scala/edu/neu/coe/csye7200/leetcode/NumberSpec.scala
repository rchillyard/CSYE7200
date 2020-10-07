package edu.neu.coe.csye7200.leetcode

import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NumberSpec extends AnyFlatSpec with Matchers {

  behavior of "NumberSpec"

  it should "toString" in {
    Zero.toString() shouldBe "0"
    Number(List(1)).toString() shouldBe "1"
    Number(List(2, 4, 3)).toString() shouldBe "2 -> 4 -> 3"
  }

  it should "apply" in {
    Zero() shouldBe 0
    Number(List(1))() shouldBe 1
    Number(List(2, 4, 3))() shouldBe 342
  }

  it should "getDigits(Long)" in {
    Number.getDigits(0L) shouldBe Seq()
    Number.getDigits(1L) shouldBe Seq(1)
    Number.getDigits(342L) shouldBe Seq(2, 4, 3)
    Number.getDigits(465L) shouldBe Seq(5, 6, 4)
    Number.getDigits(807L) shouldBe Seq(7, 0, 8)
  }

  it should "getDigits(BigInt)" in {
    Number.getDigits(BigInt(0L)) shouldBe Seq(0)
    Number.getDigits(BigInt(1L)) shouldBe Seq(1)
    Number.getDigits(BigInt(342L)) shouldBe Seq(2, 4, 3)
    Number.getDigits(BigInt(465L)) shouldBe Seq(5, 6, 4)
    Number.getDigits(BigInt(807L)) shouldBe Seq(7, 0, 8)
  }

  it should "apply(Long)" in {
    Number(1L).toString() shouldBe "1"
    Number(342L).toString() shouldBe "2 -> 4 -> 3"
    Number(465L).toString() shouldBe "5 -> 6 -> 4"
    Number(807L).toString() shouldBe "7 -> 0 -> 8"
  }

  it should "apply(BigInt)" in {
    Number(BigInt(0L)).toString() shouldBe "0"
    Number(BigInt(1L)).toString() shouldBe "1"
    Number(BigInt(342L)).toString() shouldBe "2 -> 4 -> 3"
    Number(BigInt(465L)).toString() shouldBe "5 -> 6 -> 4"
    Number(BigInt(807L)).toString() shouldBe "7 -> 0 -> 8"
  }

  it should "add" in {
    val x =     Number(342L)
    val y =     Number(465L)
    Number_Recursive.add(Nil, Zero, Zero, 0) shouldBe Seq()
    Number_Recursive.add(Nil, x, Zero, 0) shouldBe Seq(2, 4, 3)
    Number_Recursive.add(Nil, x, y, 0) shouldBe Seq(7, 0, 8)
    Number_Recursive.add(Nil, Zero, Zero, 1) shouldBe Seq(1)
    Number_Recursive.add(Nil, x, Zero, 1) shouldBe Seq(3, 4, 3)
    Number_Recursive.add(Nil, x, y, 2) shouldBe Seq(9, 0, 8)
  }

  it should "+" in {
    Zero + Zero shouldBe Zero
    Zero + Number(1L) shouldBe Number(1L)
    Number(1L) + Zero shouldBe Number(1L)
    Number(1L) + Number(1L) shouldBe Number(2L)
    Number(11L) + Number(22L) shouldBe Number(33L)
    Number(5L) + Number(5L) shouldBe Number(10L)
    Number(342) + Number(465) shouldBe Number(807)
  }

  it should "+ bigInts" in {
    val random = new scala.util.Random()
    def getRandomBigInt = BigInt(random.nextLong(Long.MaxValue))

    val bigInt1 = getRandomBigInt * getRandomBigInt
    val bigInt2 = getRandomBigInt * getRandomBigInt
    val number1 = Number(bigInt1)
    val number2 = Number(bigInt2)
    val number3 = number1 + number2
    val expected = bigInt1 + bigInt2
    number3() shouldBe expected
  }

}
