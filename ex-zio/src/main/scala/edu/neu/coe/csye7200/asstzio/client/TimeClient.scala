package edu.neu.coe.csye7200.asstzio.client

import edu.neu.coe.csye7200.asstzio.*
import java.io.IOException
import java.time.ZoneId
import scala.io.StdIn
import scala.util.matching.Regex
import zio.*
import zio.http.*
import zio.json.*

/**
 * Provides functionality to fetch and display current time details for specific time zones
 * using the World Time API.
 *
 * This object is implemented as a `ZIOAppDefault` to run as a ZIO application.
 */
object TimeClient extends ZIOAppDefault {

  /**
   * Executes the ZIO program to fetch and display the current time details for the specified timezone.
   *
   * This method performs the following actions:
   * - Prints a message indicating the start of the time-fetching process.
   * - Fetches the current time details for the predefined timezone using the `getTimeForTimezone` method.
   * - Displays the fetched time, timezone, and UTC offset information.
   * - Handles and logs any errors that occur during the process.
   *
   * @return a ZIO effect that requires an environment combining `ZIOAppArgs` and `Scope`, and
   *         either completes successfully with `Unit` or fails with an `IOException`.
   */
  def run: ZIO[ZIOAppArgs & Scope, IOException, Unit] = {
    val program: ZIO[Client, Throwable, Unit] = for {
      tz <- readTZ(s"Enter timezone (default: ${ZoneId.systemDefault}), or Area, or '?' for help: ", None)
      etc = tz.getId.startsWith("Etc/GMT")
      timezoneMessage = s"Fetching time for Timezone: $tz... " + (if (etc) "(Note that 'Etc' timezone names appear to be backwards)" else "")
      _ <- Console.printLine(timezoneMessage)
      myTime <- requestResponseJson[WorldTime](s"$worldTimeAPI/$tz")
      _ <- Console.printLine(myTime.toString)
    } yield ()

    program.provide(Client.default)
            .catchAll { error =>
              Console.printLineError(s"Error: ${error.getMessage}")
            }
  }

  /**
   * Sends an HTTP GET request to the provided URL, decodes the response body as JSON,
   * and returns the decoded result.
   *
   * This method performs the following steps:
   * - Validates the provided URL.
   * - Sends a GET request to the validated URL.
   * - Ensures the response has a status of OK (200).
   * - Confirms the response content type is JSON.
   * - Parses the response body as JSON into the specified type.
   *
   * @param u the URL to which the HTTP GET request will be sent.
   * @param j an implicit `JsonDecoder` instance to decode the response JSON into the required type `Result`.
   * @return a ZIO effect that requires a `Client` environment and either produces the decoded result of type `Result`
   *         on success or fails with a `TimeClientException` in case of error.
   */
  def requestResponseJson[Result](u: String)(implicit j: JsonDecoder[Result]): ZIO[Client, TimeClientException, Result] = ZIO.scoped {
    for {
      client <- ZIO.service[Client]
      url <- ZIO.fromEither(URL.decode(u)).orElseFail(InvalidURL(u))
      response <- client.request(Request.get(url)).mapError(ClientException.apply)
      _ <- checkOK(response.status == Status.Ok)(ApiFailure(response.status))
      _ <- checkOK(response.headers.hasJsonContentType)(LogicException("response is not JSON"))
      body <- response.body.asString.mapError(ClientException.apply)
      result <- ZIO.fromEither(body.fromJson[Result]).mapError(msg => JsonException(msg))
    } yield result
  }

  /**
   * Checks a condition and fails with the provided `TimeClientException` if the condition is not satisfied.
   * In other words, this is an assertion.
   *
   * This method evaluates the given predicate `p`, and if the result is `false`, it fails with the provided
   * exception `ex`. Otherwise, it completes successfully with `None`.
   *
   * @param p  a lazily evaluated predicate that determines whether the operation should proceed (expected to be `true`).
   * @param ex a lazily evaluated `TimeClientException` that is used as the failure reason if the predicate evaluates to `false`.
   * @return a ZIO effect that succeeds with `Option[Nothing]` if the condition is satisfied, or fails with the
   *         provided `TimeClientException` if the condition is not met.
   */
  def checkOK(p: => Boolean)(ex: => TimeClientException): ZIO[Any, TimeClientException, Option[Nothing]] = ZIO.when(!p)(ZIO.fail(ex))

  /**
   * Reads a timezone input from the user, validates and resolves it into a `ZoneId`.
   * Provides additional functionality for help and defaults based on user input.
   *
   * @param prompt      the message to display to the user as an input prompt.
   * @param maybePrefix an optional prefix to prepend to the timezone input when resolving it.
   * @return a ZIO effect that requires a `Client` environment, producing either a `ZoneId`
   *         on success or a `Throwable` in case of failure.
   */
  def readTZ(prompt: String, maybePrefix: Option[String]): ZIO[Client, Throwable, ZoneId] = for {
    _ <- Console.print(prompt)
    line <- ZIO.succeed(StdIn.readLine())
    result <- line match {
      case "" =>
        zioZoneId(ZoneId.systemDefault())
      case "?" =>
        promptAndRead("Enter the area or UTC-based zone you want: ", ZIO.succeed(helpList), None, "")
      case offsetRegex(null, null) =>
        zioZoneId(s"UTC")
      case offsetRegex(s, h) =>
        zioZoneId(s"Etc/GMT${flipSign(s)}$h")
      case tzRegex(name, _, null) =>
        val cityPrompt = "Enter the city you want: "
        maybePrefix match {
          case Some(x) if prompt == cityPrompt =>
            zioZoneId(s"$x/$name")
          case None if singleWordList.contains(name) =>
            zioZoneId(s"$name")
          case _ =>
            promptAndRead(cityPrompt, getCityList(name), Some(name), "\n")
        }
      case tzRegex(area, _, city) =>
        zioZoneId(s"$area/$city")
      case x =>
        zioZoneId(x)
    }
  } yield result

  /**
   * A predefined list of timezone-related strings and continent/region names.
   *
   * This list includes:
   * - UTC offset formats such as "UTC[+/-h]"
   * - Major continent/region names like "Africa", "America", "Asia", etc.
   * - Specific country or regional groupings such as "Brazil", "Canada", "US", etc.
   *
   * The list is primarily used for validating and assisting with timezone input.
   */
  val helpList = List("UTC[+/-h]",
    "Africa", "America", "Antarctica", "Arctic", "Asia", "Atlantic", "Australia", "Brazil", "Canada", "Chile", "Europe", "Indian", "Mexico", "Pacific", "Poland", "Turkey", "US")

  /**
   * A predefined list of time zone identifiers or aliases that consist of single words.
   *
   * These identifiers are commonly used to specify time zones in various contexts,
   * including APIs, user inputs, and system configurations.
   */
  val singleWordList = List("CET", "CST6CDT", "Cuba", "EET", "EST", "EST5EDT", "Egypt", "Eire", "GB",
    "GB-Eire", "GMT", "GMT+0", "GMT-0", "GMT0", "Greenwich", "HST", "Hongkong", "Iceland", "Iran", "Israel",
    "Jamaica", "Japan", "Kwajalein", "Libya", "MET", "MST", "MST7MDT", "NZ", "NZ-CHAT", "Navajo",
    "PRC", "PST8PDT", "Poland", "Portugal", "ROC", "ROK", "Singapore", "Turkey", "UCT", "Universal", "W-SU", "WET", "Zulu")

  val tzRegex: Regex = """([a-zA-Z0-9_]+)(/([a-zA-Z0-9_]+))?""".r

  val offsetRegex: Regex = """UTC([+-])?(\d{1,2})?""".r

  /**
   * Retrieves a list of city names based on the specified category name.
   * This method fetches raw city data from the World Time API,
   * parses the response, and processes the list to return cleaned city names.
   *
   * @param name the category name used to filter and retrieve city names from the API.
   *             The response from the API is expected to have city names prefixed with this category,
   *             which will be stripped in the final result.
   * @return a ZIO effect that produces a list of strings representing city names,
   *         after removing the category prefix.
   */
  def getCityList(name: String): ZIO[Client, TimeClientException, List[String]] =
    requestResponseJson[List[String]](s"$worldTimeAPI/$name").map(x => x.map(s => s.replace(s"$name/", "")))

  /**
   * Determines the opposite sign for a given string representing a sign.
   * This is required for handling the Etc time zones (please see FYI of worldtimeapi.org).
   *
   * This method inspects the input string to determine if it starts with a
   * negative sign ("-"). If so, it returns "+"; otherwise, it returns "-".
   *
   * @param s the input string representing a sign, expected to start with either "-" or "+".
   * @return a string representing the opposite sign ("+" if the input starts with "-", otherwise "-").
   */
  private def flipSign(s: String): String = if (s.startsWith("-")) "+" else "-"

  /**
   * Wraps a `ZoneId` instance into a ZIO effect.
   *
   * This method takes a `ZoneId` and produces a ZIO effect that succeeds
   * immediately with the provided `ZoneId`.
   *
   * @param zoneId the `ZoneId` to be wrapped into a ZIO effect.
   * @return a ZIO effect that completes successfully with the given `ZoneId`.
   */
  private def zioZoneId(zoneId: ZoneId): ZIO[Any, Nothing, ZoneId] = ZIO.succeed(zoneId)

  /**
   * Resolves a given timezone string into a `ZoneId` wrapped in a ZIO effect.
   *
   * This method attempts to create a `ZoneId` from the provided timezone string.
   *
   * @param zoneId the timezone string to be resolved into a `ZoneId`.
   * @return a ZIO effect that produces a `ZoneId` without requiring any environment
   *         or possibility of failure.
   */
  private def zioZoneId(zoneId: String): ZIO[Any, Nothing, ZoneId] = zioZoneId(ZoneId.of(zoneId))

  /**
   * Prompts the user for input, displays a formatted list of options, and reads a timezone-related input.
   *
   * @param select      a string that specifies the prompt label to be displayed to the user.
   * @param zioList     a ZIO effect that produces a list of strings representing available options.
   * @param maybePrefix an optional prefix to prepend to the user's input if applicable.
   * @param openDelim   a string used as the opening delimiter for formatting the list in the displayed output.
   * @return a ZIO effect that performs the described prompting and reading workflow, resulting in the user's input or
   *         an effectful value processed through the `readTZ` method.
   */
  private def promptAndRead(select: String, zioList: ZIO[Client, Throwable, List[String]], maybePrefix: Option[String], openDelim: String) = {
    for {
      list <- zioList
      _ <- Console.print(list.mkString(openDelim, "\n", "\n"))
      result <- readTZ(select, maybePrefix)
    } yield result
  }

  private val worldTimeAPI = "https://worldtimeapi.org/api/timezone"

}

/**
 * Represents an exception that occurs in the `TimeClient` application, typically when
 * interacting with remote APIs or due to errors in logic or data processing.
 *
 * This is an abstract base class for more specific exception types that provide details
 * about the nature of the failure. Subclasses of `TimeClientException` may include issues
 * like invalid URLs, API failures, JSON parsing errors, or unexpected client issues.
 *
 * @param msg   the error message describing the exception.
 * @param cause the underlying cause of the exception, if any.
 */
abstract class TimeClientException(msg: String, cause: Throwable) extends Exception(msg, cause)

/**
 * Represents an exception that denotes an API call failure.
 *
 * This exception is used to encapsulate the HTTP response status
 * when an API request fails. It is a subclass of `TimeClientException`,
 * enabling it to be seamlessly integrated into use cases requiring
 * the handling of time-related exceptions.
 *
 * @param status the HTTP response status indicating the nature of the API failure.
 */
case class ApiFailure(status: Status) extends TimeClientException(s"API request failed: $status", null)

/**
 * Represents an exception indicating that a provided URL is invalid.
 *
 * This case class is utilized in scenarios where the URL provided to a function
 * or method, such as an API call, fails to meet the required validation criteria.
 *
 * @param url the invalid URL string that caused the exception to be thrown.
 */
case class InvalidURL(url: String) extends TimeClientException(s"Invalid URL: $url", null)

/**
 * Represents an exception that occurs when a JSON parsing operation fails.
 *
 * This exception is a specialized subclass of `TimeClientException` and is typically used to
 * encapsulate errors related to decoding or parsing JSON during API responses.
 *
 * The exception message is prefixed with "JSON parsing error:" to indicate the nature of the failure.
 *
 * @param msg the specific error message describing the JSON parsing failure.
 */
case class JsonException(msg: String) extends TimeClientException(s"JSON parsing error: $msg", null)

/**
 * Represents an exception thrown when a logical error occurs within the application's context.
 *
 * This class extends `TimeClientException` and provides an error message specific to logical errors.
 * Such exceptions are distinct from runtime exceptions caused by unexpected states or conditions.
 *
 * @constructor Creates a `LogicException` with a specific error message.
 * @param msg The error message that describes the logical error.
 */
case class LogicException(msg: String) extends TimeClientException(s"logic error: $msg", null)

/**
 * Represents an exception specific to the client operations.
 *
 * This exception is a wrapper around a `Throwable` and is used to encapsulate
 * unexpected errors that occur during client operations or interactions. It extends
 * the `TimeClientException` to provide more specific context regarding client errors.
 *
 * @param throwable the underlying cause of the exception, encapsulated as a `Throwable`.
 */
case class ClientException(throwable: Throwable) extends TimeClientException(s"unexpected throwable", throwable)