package edu.neu.coe.csye7200.assthw2.timeapi

import edu.neu.coe.csye7200.assthw2.Tries.{tryEquals, tryNotEquals}
import scala.util.{Failure, Try}
import spray.json.*

/**
 * Main method to retrieve the current time for a specific timezone and display the result.
 * The method utilizes an implicit timezone and fetches the time from an external API.
 * It also logs any failures encountered during the operation.
 *
 * @return Unit - This method does not return a value, but handles side effects such as logging failures or displaying the time.
 */
@main
def doWhatsTheTime(): Unit = {

    def getTheTime(using timezone: String): Try[Time] = {
        import TimeJsonProtocol.given

        for {
          // NOTE this is no longer serving time requests
          response <- Try(requests.get("http://timeapi.io/api/time/current/zone?timeZone=" + timezone))
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

/**
 * Provides JSON (de)serialization support for the `Time` case class.
 * Extends the `DefaultJsonProtocol` to define implicit conversions between
 * JSON and the `Time` case class using `jsonFormat3`. The implementation assumes
 * that `Time` has exactly three parameters: `dateTime`, `dayOfWeek`, and `dstActive`.
 *
 * This protocol is intended to be imported and used whenever JSON formatting
 * or parsing is required for instances of `Time`, especially in client-server
 * interactions or when working with external APIs that return or accept time information
 * in JSON format.
 */
object TimeJsonProtocol extends DefaultJsonProtocol {
    given timeFormat: RootJsonFormat[Time] = jsonFormat3(Time.apply)
}