package edu.neu.coe.csye7200.assthw2.worldtimeapi

import edu.neu.coe.csye7200.assthw2.Tries.{tryEquals, tryNotEquals}
import edu.neu.coe.csye7200.assthw2.worldtimeapi.WhatsTheTime.maybeTime
import scala.util.{Failure, Try}
import spray.json.*

@main def getTheTime(): Unit = {
  // show the result
  maybeTime foreach println
  // log any failures.
  maybeTime.recoverWith { case x: Exception => System.err.println(s"Failure: ${x.getLocalizedMessage}"); Failure(x) }
}

/**
 * The `WhatsTheTime` object provides functionality to fetch and output the current time for a specified timezone
 * using the World Time API. It uses the `Try` monad to handle potential errors during execution, ensuring resilience
 * against invalid API responses or other runtime exceptions.
 *
 * This object is deprecated in favor of the zio module.
 */
object WhatsTheTime {

  val maybeTime: Try[Time] = getTheTime("America/New_York")

  /**
     * Retrieves the current time for a given timezone using the World Time API.
     *
     * @param timezone The timezone for which the current time is requested, specified as a string (e.g., "America/New_York").
     * @return A `Try[Time]` object containing either the current time in the specified timezone if the request is successful,
     *         or a `Failure` with the appropriate error if the request fails.
     */
    def getTheTime(timezone: String): Try[Time] = {
      import TimeJsonProtocol.given

      for {
        response <- Try(requests.get("http://worldtimeapi.org/api/timezone/" + timezone))
            _ <- tryEquals(response.statusCode, 200, "invalid status")
            _ <- tryEquals(response.headers("content-type"), List("application/json; charset=utf-8"), "bad content type")
            json <- tryNotEquals(response.text(), "", "empty json")
            time <- Try(json.parseJson.convertTo[Time])
        } yield time
    }
}

case class Time(abbreviation: String, datetime: String, day_of_week: Int, dst: Boolean):
    override def toString: String = s"$datetime $abbreviation ${if (dst) "Daylight Saving Time" else ""}"

object TimeJsonProtocol extends DefaultJsonProtocol:
  given timeFormat: RootJsonFormat[Time] = jsonFormat4(Time.apply)
