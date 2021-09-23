package edu.neu.coe.csye7200.fp.minidatabase2

import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util._

/**
  * @author scalaprof
  */
class MiniDatabaseSpec2 extends AnyFlatSpec with Inside with Matchers {

  "map2" should "succeed for two Success values" in {
    val t1 = Success(1)
    val t2 = Success(2)
    val t3 = MiniDatabase2.map2(t1, t2) { case (x, y) => x + y }
    t3 should matchPattern { case Success(3) => }
  }
  it should "fail if any Failures" in {
    val t0 = Failure[Int](new IllegalArgumentException)
    val t1 = Success(1)
    val t2 = Success(2)
    MiniDatabase2.map2(t0, t2) { case (x, y) => x + y } should matchPattern { case Failure(_) => }
    MiniDatabase2.map2(t0, t1) { case (x, y) => x + y } should matchPattern { case Failure(_) => }
    MiniDatabase2.map2(t2, t0) { case (x, y) => x + y } should matchPattern { case Failure(_) => }
  }
  "map3" should "succeed for two Some values" in {
    val t1 = Some(1)
    val t2 = Some(2)
    val t3 = Some(3)
    MiniDatabase2.map3(t1, t2, t3) { case (x, y, z) => x + y + z } should matchPattern { case Some(6) => }
  }
  it should "fail if any None values" in {
    val t1 = Some(1)
    val t2 = None
    val t3 = Some(3)
    MiniDatabase2.map3(t1, t2, t3) { case (x, y, z) => x + y + z } should matchPattern { case None => }
  }
  it should "fail if any Failures" in {
    val t0 = Failure[Int](new IllegalArgumentException)
    val t1 = Success(1)
    val t2 = Success(2)
    MiniDatabase2.map2(t0, t2) { case (x, y) => x + y } should matchPattern { case Failure(_) => }
    MiniDatabase2.map2(t0, t1) { case (x, y) => x + y } should matchPattern { case Failure(_) => }
    MiniDatabase2.map2(t2, t0) { case (x, y) => x + y } should matchPattern { case Failure(_) => }
  }
  "Height" should "succeed 6 ft 5 in" in {
    Height.parse("6 ft 5 in") should matchPattern { case Success(_) => }
  }
  it should "fail 6 ft five in" in {
    Height.parse("6 ft five in") should matchPattern { case Failure(_) => }
  }
  it should """succeed 6' 5"""" in {
    Height.parse("""6' 5"""") should matchPattern { case Success(_) => }
  }
  it should """fail to parse 6'""" in {
    Height.parse("""6'""") should matchPattern { case Failure(_) => }
  }
  it should "succeed: equal 77 inches and be considered tall" in {
    val height = Height.parse("6 ft 5 in")
    inside(height) {
      case Success(h) => h should matchPattern { case Height(6, 5) => }
    }
    inside(height) {
      case Success(h) => h.inches shouldEqual 77
    }
    inside(height) {
      case Success(h) => MiniDatabase2.measure(h) should be("tall")
    }
  }

  "Name" should "succeed: Tom Brady" in {
    Name.parse("Tom Brady") should matchPattern { case Success(_) => }
  }

  it should """succeed: Thomas E. P. "Tom" Brady""" in {
    Name.parse("""Thomas E. P. "Tom" Brady""") should matchPattern { case Success(_) => }
  }

  "Entry" should """succeed: Thomas E. P. "Tom" Brady, etc.""" in {
    Entry.parse("""Thomas E. P. "Tom" Brady, 078-05-1120, Aug 3rd 1977, 6 ft 4 in, 225""".split(",")) should matchPattern { case Success(_) => }
  }

  it should """fail: Thomas E. P. "Tom" Brady""" in {
    Entry.parse("""Brady, 123-45-6789""".split(",")) should matchPattern { case Failure(_) => }
  }
}
