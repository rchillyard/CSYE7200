package edu.neu.coe.csye7200.assthw

import edu.neu.coe.csye7200.assthw.Tries.{tryEquals, tryNotEquals}
import scala.util.{Failure, Success, Try}
import spray.json._

object WhatsTheTime extends App {

    def getTheTime(timezone: String): Try[Time] = {
        import TimeJsonProtocol._

        for {
            response <- Try(requests.get("https://worldtimeapi.org/api/timezone/" + timezone))
            _ <- tryEquals(response.statusCode, 200, "invalid status")
            _ <- tryEquals(response.headers("content-type"), List("application/json; charset=utf-8"), "bad content type")
            json <- tryNotEquals(response.text(), "", "empty json")
            time <- Try(json.parseJson.convertTo[Time])
        } yield time
    }

    val maybeTime = getTheTime("America/New_York")

// show the result
    maybeTime foreach println
    // log any failures.
    maybeTime.recoverWith { case x: Exception => System.err.println(s"Failure: ${x.getLocalizedMessage}"); Failure(x) }
}

case class Time(abbreviation: String, datetime: String, day_of_week: Int, dst: Boolean) {
    override def toString: String = s"$datetime $abbreviation ${if (dst) "Daylight Saving Time" else ""}"
}

object TimeJsonProtocol extends DefaultJsonProtocol {
    implicit val timeFormat: RootJsonFormat[Time] = jsonFormat4(Time.apply)
}

object Tries {
    def tryMatch[X](f: (X, X) => Boolean)(x: => X, expected: X, message: String): Try[X] =
        if (f(x, expected)) Success(x) else Failure(new Exception(s"$message: $x did not equal $expected"))

    def tryEquals[X](x: => X, expected: X, message: String): Try[X] = tryMatch[X](_ == _)(x, expected, message)

    def tryNotEquals[X](x: => X, expected: X, message: String): Try[X] = tryMatch[X](_ != _)(x, expected, message)
}