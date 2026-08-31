package edu.neu.coe.csye7200.asstzio

import java.time.{LocalDateTime, ZoneId, ZoneOffset}
import zio.json.*

/**
 * Represents a specific point in time with additional metadata such as timezone and calendar information.
 *
 * @param datetime    the date and time in the local representation.
 * @param timezone    the name of the timezone where this date and time is applicable.
 * @param utc_offset  the UTC offset of the specified timezone.
 * @param day_of_year the day of the year (1 to 366) corresponding to the date.
 * @param week_number the ISO week number corresponding to the date.
 */
case class WorldTime(datetime: LocalDateTime,
                     timezone: String,
                     utc_offset: ZoneOffset,
                     day_of_year: Int,
                     week_number: Int
                    ):
  def daysInYear: Int = if (datetime.toLocalDate.isLeapYear) 366 else 365

  override def toString: String = s"Datetime for $timezone: $datetime ($utc_offset); $week_number/52, $day_of_year/$daysInYear"

/**
 * Companion object for the WorldTime case class.
 *
 * Provides JSON encoding and decoding functionality for the `WorldTime` case class
 * using implicit `JsonEncoder` and `JsonDecoder` derivations.
 */
object WorldTime {

  import java.time.OffsetDateTime
  import scala.util.Try

  given JsonDecoder[WorldTime] =
    DeriveJsonDecoder.gen[WorldTimeRaw].mapOrFail { r =>
      Try(WorldTime(OffsetDateTime.parse(r.datetime).toLocalDateTime, r.timezone, ZoneOffset.of(r.utc_offset), r.day_of_year, r.week_number))
              .toEither.left.map(_.getMessage)
    }

  /**
   * Retrieves a `TimeZone` object based on the provided timezone string.
   *
   * If the input string is empty, the system's default timezone is returned.
   * Otherwise, it attempts to retrieve the specified timezone.
   *
   * @param tz the name of the desired timezone, or an empty string to indicate the default timezone.
   * @return the corresponding `TimeZone` object for the input string. If the input is empty,
   *         the system default `TimeZone` is returned.
   */
  def getTZ(tz: String): ZoneId =
    if (tz.isEmpty) ZoneId.systemDefault() else ZoneId.of(tz)

  // Decode from JSON object fields, not a JSON array/tuple, and fail safely on parse errors.
  private case class WorldTimeRaw(datetime: String,
                                  timezone: String,
                                  utc_offset: String,
                                  day_of_year: Int,
                                  week_number: Int)
}
