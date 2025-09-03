package edu.neu.coe.csye7200.assthw

import edu.neu.coe.csye7200.assthw.worldtimeapi.Time
import edu.neu.coe.csye7200.assthw.worldtimeapi.WhatsTheTime.getTheTime
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.Success

class WhatsTheTimeSpec extends AnyFlatSpec with should.Matchers {

    behavior of "WhatsTheTimeSpec"

    // TESTME use https://timeapi.io/swagger/index.html instead.
    ignore should "getTheTime" in {
        val maybeTime = getTheTime("America/New_York")
        maybeTime should matchPattern { case Success(Time(_, _, _, _)) => }
    }

}