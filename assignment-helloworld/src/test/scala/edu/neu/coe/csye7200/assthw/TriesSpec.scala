package edu.neu.coe.csye7200.assthw

import edu.neu.coe.csye7200.assthw.Tries.{tryEquals, tryMatch, tryNotEquals}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.Success

class TriesSpec extends AnyFlatSpec with should.Matchers {

    behavior of "TriesSpec"

    it should "tryEquals" in {
        tryEquals(1, 1, "one") shouldBe Success(1)
    }

    it should "tryMatch" in {
        tryMatch[Int](_ + _ == 2)(1, 1, "1 and 1") shouldBe Success(1)
    }

    it should "tryNotEquals" in {
        tryNotEquals(1, 2, "one/two") shouldBe Success(1)

    }

}