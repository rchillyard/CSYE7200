package edu.neu.coe.csye7200.lbsort

import edu.neu.coe.csye7200.lbsort.Comparison._
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.language.postfixOps

/**
  * @author scalaprof
  */
class ComparisonSpec extends AnyFlatSpec with Matchers with Futures with ScalaFutures {

  behavior of "Comparison"

  it should "apply(Int)" in {
    Comparison(0) shouldBe Same
    Comparison(1) shouldBe more
    Comparison(-1) shouldBe less
  }
  it should "toInt" in {
    Comparison(-1).toInt shouldBe -1
    Comparison(0).toInt shouldBe 0
    Comparison(1).toInt shouldBe 1
  }
  it should "apply(Option[Boolean])" in {
    Comparison(None) shouldBe Same
    Comparison(Some(false)) shouldBe more
    Comparison(Some(true)) shouldBe less
  }
  it should "flip" in {
    more.flip shouldBe less
    less.flip shouldBe more
    Same.flip shouldBe Same
  }
  it should "orElse" in {
    more orElse more shouldBe more
    more orElse less shouldBe more
    less orElse more shouldBe less
    less orElse less shouldBe less
    Same orElse less shouldBe less
    Same orElse more shouldBe more
    Same orElse Same shouldBe Same
  }
  it should "implement | correctly" in {
    more | Same shouldBe more
    Same | more shouldBe more
    less | Same shouldBe Same
    Same | less shouldBe Same
    more | more shouldBe more
    more | less shouldBe more
    less | more shouldBe more
    Same | Same shouldBe Same
    less | less shouldBe less
  }
  it should "implement & correctly" in {
    more & Same shouldBe Same
    Same & more shouldBe Same
    less & Same shouldBe less
    Same & less shouldBe less
    more & more shouldBe more
    more & less shouldBe less
    less & more shouldBe less
    Same & Same shouldBe Same
    less & less shouldBe less
  }

  it should "implement || correctly" in {
    more || Same shouldBe more
    Same || more shouldBe more
    less || Same shouldBe Same
    Same || less shouldBe Same
    more || more shouldBe more
    more || less shouldBe more
    less || more shouldBe more
    Same || Same shouldBe Same
    less || less shouldBe less
  }
  it should "implement && correctly" in {
    more && Same shouldBe Same
    Same && more shouldBe Same
    less && Same shouldBe less
    Same && less shouldBe less
    more && more shouldBe more
    more && less shouldBe less
    less && more shouldBe less
    Same && Same shouldBe Same
    less && less shouldBe less
  }
}