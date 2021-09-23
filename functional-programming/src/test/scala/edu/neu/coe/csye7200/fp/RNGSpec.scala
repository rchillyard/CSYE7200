package edu.neu.coe.csye7200.fp

import edu.neu.coe.csye7200._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.language.postfixOps

/**
  * @author scalaprof
  */
class RNGSpec extends AnyFlatSpec with Matchers {

  //noinspection SimplifiableFoldOrReduce
  // XXX not quite sure why I insisted on using reduceLeft here. Could just use xs.sum
  def sum(xs: Seq[Double]): Double = xs.reduceLeft(_ + _)

  // you must use reduceLeft here...
  def stdDev(xs: Seq[Double]): Double = math.sqrt(xs.reduceLeft((a, x) => a + x * x)) / xs.length

  // ...and here
  private def mean(xs: Seq[Double]) = sum(xs) / xs.length

  // Clearly, this doesn't look good. We will soon learn how to write
  // generic methods like sum and mean. But for now, this is what we've got.
  def sumU(xs: Seq[UniformDouble]): Double = xs.foldLeft(0.0)((a, x) => x + a)

  private def meanU(xs: Seq[UniformDouble]) = sumU(xs) / xs.length

  "RNG(0L)" should "match case RNG(-4962768465676381896L)" in {
    val r: RNG[Long] = LongRNG(0L)
    r.next should matchPattern { case LongRNG(-4962768465676381896L) => }
  }
  it should "match case RNG(4804307197456638271L) on next" in {
    val r: RNG[Long] = LongRNG(0L)
    r.next.next should matchPattern { case LongRNG(4804307197456638271L) => }
  }
  "7th element of RNG(0)" should "match case RNG(-4962768465676381896L)" in {
    val lrs = RNG.rngs(LongRNG(0)) take 7 toList;
    (lrs last) should matchPattern { case LongRNG(488730542833106255L) => }
  }
  "Double stream" should "have zero mean" in {
    val xs = RNG.values(DoubleRNG(0)) take 1001 toList;
    math.abs(mean(xs)) shouldBe <=(5E-3)
  }
  "0..1 stream" should "have mean = 0.5" in {
    val xs = RNG.values(UniformDoubleRNG(0)) take 1001 toList;
    math.abs(meanU(xs) - 0.5) shouldBe <=(5E-3)
  }
  "Gaussian stream" should "have zero mean" in {
    val xs = RNG.values2(GaussianRNG(0)) take 11111 toList;
    math.abs(mean(xs)) shouldBe <=(5E-3)
  }
  it should "have unit std. deviation" in {
    val xs = RNG.values2(GaussianRNG(0)) take 11111 toList;
    (math.abs(stdDev(xs)) - 1) shouldBe <=(5E-3)
  }
}
