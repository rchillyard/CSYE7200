package edu.neu.coe.csye7200

import java.util.Date

import org.scalatest.{FlatSpec, Matchers}

/**
  * @author scalaprof
  */
class LocaleDependentSpec extends FlatSpec with Matchers {

  "today" should "equal today" in {
    //noinspection ScalaDeprecation
    val x = ScalaDate.apply(new Date(2015 - 1900, 9, 1))
    x.toString shouldBe "1 octobre 2015"
  }

}
