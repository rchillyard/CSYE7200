package edu.neu.coe.csye7200.fp

import edu.neu.coe.csye7200.ScalaDate
import java.util.Date
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * @author scalaprof
  */
class LocaleDependentSpec extends AnyFlatSpec with Matchers {

  "today" should "equal today" in {
    //noinspection ScalaDeprecation
    val x = ScalaDate.apply(new Date(2015 - 1900, 9, 1))
    x.toString shouldBe "1 octobre 2015"
  }

}
