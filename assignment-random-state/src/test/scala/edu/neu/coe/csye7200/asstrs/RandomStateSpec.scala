package edu.neu.coe.csye7200.asstrs

import org.scalatest.{FlatSpec, Matchers}

import scala.language.postfixOps
import scala.util.Random

/**
  * @author scalaprof
  */
class RandomStateSpec extends FlatSpec with Matchers {

  private def stdDev(xs: Seq[Double]): Double = math.sqrt(xs.reduceLeft((a, x) => a + x * x)) / xs.length

  private def mean(xs: Seq[Double]) = xs.sum / xs.length

  // XXX Clearly, this doesn't look good. We will soon learn how to write
  // generic methods like sum and mean. But for now, this is what we've got.
  def sumU(xs: Seq[UniformDouble]): Double = xs.foldLeft(0.0)((a, x) => (a + x.x))

  def meanU(xs: Seq[UniformDouble]) = sumU(xs) / xs.length

  "RandomState(0L)" should "match case RandomState(4804307197456638271)" in {
    val r: RandomState[Long] = RandomState(0L)
    r.next should matchPattern { case JavaRandomState(4804307197456638271L,_) => }
  }
  it should "match case RandomState(-1034601897293430941) on next" in {
    val r: RandomState[Long] = RandomState(0L)
    r.next.next should matchPattern { case JavaRandomState(-1034601897293430941L,_) => }
    // why doesn't the following work?
//    r.next.next.asInstanceOf[JavaRandomState[Long]].g shouldBe identity
    // e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
  }
  "7th element of RandomState(0)" should "match case RandomState(5082315122564986995L)" in {
    val lrs = RandomState(0).toStream.slice(6, 7)
    (lrs head) should matchPattern { case 5082315122564986995L => }
  }
  "longToDouble" should "work" in {
    val max = RandomState.longToDouble(Long.MaxValue)
    max shouldBe 1.0 +- 1E-6
    val min = RandomState.longToDouble(Long.MinValue)
    min shouldBe -1.0 +- 1E-6
    val value = RandomState.longToDouble(3487594572834985L)
    value shouldBe 3.7812576126163456E-4 +- 1E-6
  }
  "0..1 stream" should "have mean = 0.5" in {
    val xs = RandomState(0).map(RandomState.longToDouble).map(RandomState.doubleToUniformDouble).toStream take 1001 toList;
    meanU(xs) shouldBe 0.5 +- 5E-3
  }
  "BetterRandomState" should "have mean = 0.5" in {
    val xs = BetterRandomState(0,BetterRandomState.hDouble).toStream take 1001 toList;
    mean(xs) shouldBe 0.5 +- 5E-3
  }
  "map" should "work" in {
    val rLong: RandomState[Long] = RandomState(0)
    val rInt = rLong.map(_.toInt)
    rInt.get shouldBe -723955400
    val next = rInt.next
    next.get shouldBe 406937919
    val next2 = next.next
    next2.get shouldBe 1407270755
  }
  it should "work with map of map" in {
    val rLong: RandomState[Long] = RandomState(0L)
    val rInt = rLong.map(_.toInt)
    val rBoolean = rInt.map(_ % 2 == 0)
    rBoolean.get shouldBe true
  }
  "flatMap" should "work" in {
    val r1 = RandomState(0)
    val r2 = r1.flatMap(RandomState(_))
    r2.get shouldBe 4804307197456638271L
  }

  "for comprehension" should "work" in {
    val r1 = RandomState(0)
    val z: RandomState[Double] = for (x <- r1; _ <- RandomState(x)) yield x.toDouble/Long.MaxValue
    z.get shouldBe -0.5380644352028887 +- 1E-6
  }
}
