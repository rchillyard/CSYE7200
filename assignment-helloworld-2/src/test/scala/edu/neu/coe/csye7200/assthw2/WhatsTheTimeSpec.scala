package edu.neu.coe.csye7200.assthw2

import edu.neu.coe.csye7200.assthw2.worldtimeapi.Time
import edu.neu.coe.csye7200.assthw2.worldtimeapi.WhatsTheTime.getTheTime
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.Success

class WhatsTheTimeSpec extends AnyFlatSpec with should.Matchers {

    behavior of "WhatsTheTime"

    // TESTME use http://timeapi.io/swagger/index.html instead.
    it should "getTheTime" in {
        val maybeTime = getTheTime("America/New_York")
        maybeTime should matchPattern { case Success(Time(_, _, _, _)) => }
    }

}