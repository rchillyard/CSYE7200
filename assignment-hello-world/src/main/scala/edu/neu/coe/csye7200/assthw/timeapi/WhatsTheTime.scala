package edu.neu.coe.csye7200.assthw.timeapi

import edu.neu.coe.csye7200.assthw.Tries.{tryEquals, tryNotEquals}
import io.circe.generic.auto.*
import io.circe.parser.decode
import scala.util.{Failure, Try}

/**
 * Main method to retrieve the current time for a specific timezone and display the result.
 * The method utilizes an implicit timezone and fetches the time from an external API.
 * It also logs any failures encountered during the operation.
 *
 * @return Unit - This method does not return a value, but handles side effects such as logging failures or displaying the time.
 */
@main
def doWhatsTheTime(): Unit = {

    def getTheTime(using timezone: String): Try[Time] =
        for {
            // NOTE this is no longer serving time requests
            response <- Try(requests.get("http://timeapi.io/api/time/current/zone?timeZone=" + timezone))
            _ <- tryEquals(response.statusCode, 200, "invalid status")
            _ <- tryEquals(response.headers("content-type"), List("application/json; charset=utf-8"), "bad content type")
            json <- tryNotEquals(response.text(), "", "empty json")
            time <- decode[Time](json).toTry
        } yield time

    implicit val tz: String = "America/New_York"

    val maybeTime = getTheTime

    // show the result
    maybeTime foreach println
    // log any failures.
    maybeTime.recoverWith { case x: Exception => System.err.println(s"Failure: ${x.getLocalizedMessage}"); Failure(x) }
}

/**
 * Represents a time instance containing the date and time information,
 * the day of the week, and whether daylight saving time is active.
 *
 * @constructor Creates an instance of `Time`.
 * @param dateTime  The date and time in string format.
 * @param dayOfWeek The day of the week in string format.
 * @param dstActive Indicates whether daylight saving time is active.
 */
case class Time(dateTime: String, dayOfWeek: String, dstActive: Boolean) {
    override def toString: String = s"$dateTime ${if (dstActive) "Daylight Saving Time" else ""}"
}