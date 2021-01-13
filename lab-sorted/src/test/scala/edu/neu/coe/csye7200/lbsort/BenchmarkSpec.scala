package edu.neu.coe.csye7200.lbsort

import edu.neu.coe.csye7200.benchmark._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

/**
  * TryWithSpec based on: http://codereview.stackexchange.com/questions/79267/scala-trywith-that-closes-resources-automatically
  * Created by scalaprof on 8/5/16.
  */
class BenchmarkSpec extends AnyFlatSpec with Matchers {
  "Benchmark--don't worry if this fails tests under debug or coverage" should "yield correct number of nanoseconds" taggedAs Slow in {
    val nanos = 10000.times(Factorial.factorial(40))
    // NOTE: this might need to be varied according to the speed of the machine, etc.
    nanos shouldBe 10000.0 +- 9000
  }
}
