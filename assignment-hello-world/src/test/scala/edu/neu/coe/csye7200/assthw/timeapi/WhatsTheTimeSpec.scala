//package edu.neu.coe.csye7200.assthw.timeapi
//
//import edu.neu.coe.csye7200.assthw.openmeteo.Time
//import edu.neu.coe.csye7200.assthw.openmeteo.Forecast1.getTheTime
//import org.scalatest.flatspec.AnyFlatSpec
//import org.scalatest.matchers.should
//import scala.util.Success
//
//class WhatsTheTimeSpec extends AnyFlatSpec with should.Matchers {
//
//    behavior of "Forecast1"
//
//    // TESTME use http://timeapi.io/swagger/index.html instead.
//    it should "getTheTime" in {
//        val maybeTime = getTheTime("America/New_York")
//        maybeTime match {
//            case Success(Time(_, _, _, _)) => succeed
//            case scala.util.Failure(e) => cancel(s"API unavailable: ${e.getMessage}")
//        }
//    }
//}
