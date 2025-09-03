package edu.neu.coe.csye7200.assthw.timeapi

import edu.neu.coe.csye7200.assthw.Tries.{tryEquals, tryNotEquals}
import scala.util.{Failure, Try}
import spray.json._

object WhatsTheTime extends App {

    def getTheTime(implicit timezone: String): Try[Time] = {
        import TimeJsonProtocol._

        for {
            response <- Try(requests.get("https://timeapi.io/api/time/current/zone?timeZone=" + timezone))
            _ <- tryEquals(response.statusCode, 200, "invalid status")
            _ <- tryEquals(response.headers("content-type"), List("application/json; charset=utf-8"), "bad content type")
            json <- tryNotEquals(response.text(), "", "empty json")
            time <- Try(json.parseJson.convertTo[Time])
        } yield time
    }

    implicit val tz: String = "America/New_York"

    val maybeTime = getTheTime

// show the result
    maybeTime foreach println
    // log any failures.
    maybeTime.recoverWith { case x: Exception => System.err.println(s"Failure: ${x.getLocalizedMessage}"); Failure(x) }
}

case class Time(dateTime: String, dayOfWeek: String, dstActive: Boolean) {
    override def toString: String = s"$dateTime ${if (dstActive) "Daylight Saving Time" else ""}"
}

object TimeJsonProtocol extends DefaultJsonProtocol {
    implicit val timeFormat: RootJsonFormat[Time] = jsonFormat3(Time.apply)
}