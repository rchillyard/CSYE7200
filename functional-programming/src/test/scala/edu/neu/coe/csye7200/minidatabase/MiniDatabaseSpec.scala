package edu.neu.coe.csye7200.minidatabase

import org.scalatest.{FlatSpec, Matchers}

/**
  * @author scalaprof
  */
class MiniDatabaseSpec extends FlatSpec with Matchers {

  "Height" should "parse 6 ft 5 in" in {
    Height("6 ft 5 in")
  }
  it should """parse 6' 5"""" in {
    Height("""6' 5"""")
  }
  it should "equal 77 inches" in {
    Height("6 ft 5 in").inches should be(77)
  }
  it should "be considered tall" in {
    MiniDatabase.measure(Height("6 ft 5 in")) should be("tall")
  }

}
