package edu.neu.coe.csye7200.assthw.openmeteo

import edu.neu.coe.csye7200.assthw.Tries.{tryEquals, tryNotEquals}
import edu.neu.coe.csye7200.assthw.openmeteo.WeatherResponse.{currentWeatherQuery, urlForecast}
import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*
import requests.Response
import scala.util.{Failure, Success, Try}

case class CurrentWeather(temperature: Double, time: String, windspeed: Double, winddirection: Double):
  override def toString: String =
  s"""
     |  local time: $time (as of the most recent 15-minute update)
     |  temperature: $temperature°C
     |  wind: $windspeed km/h @ $winddirection°""".stripMargin

case class WeatherResponse(latitude: Double, longitude: Double, current_weather: CurrentWeather):
  override def toString: String =
    s"($latitude, $longitude): $current_weather"

object WeatherResponse:
  val urlForecast = "https://api.open-meteo.com/v1/forecast?"
  val currentWeatherQuery = "&current_weather=true"

object Forecast0:
  def showConditions(location: String, city: String, timezone: String): Unit =
    val response = requests.get(urlForecast + location + currentWeatherQuery + "&timezone=" + timezone)
    if (response.statusCode == 200)
    then
      if (response.headers("content-type").contains("application/json; charset=utf-8"))
      then println(s"$city Conditions: ${response.text()}")
      else System.err.println(s"""bad content type: ${response.headers("content-type")}""")
    else System.err.println(s"Bad status code: ${response.statusCode}")

@main def whatAreTheBostonConditions0(): Unit =
  Forecast0.showConditions("latitude=42.36&longitude=-71.06", s"Boston", "America/New_York")

object Forecast1:

  def getResponse(location: String): Try[Response] =
    for
      response <- Try(requests.get(urlForecast + location + currentWeatherQuery))
      _ <- tryEquals(response.statusCode, 200, "invalid status")
      _ <- tryEquals(response.headers("content-type"), List("application/json; charset=utf-8"), "bad content type")
    yield response

  def getJson(response: Response): Try[String] =
    for
      json <- Try(response.text())
      _ = println(json)
      _ <- tryNotEquals(json, "", "empty json")
    yield json

  def getConditions(location: String, timezone: String, city: String): Try[String] =
    for
      response <- Try(requests.get(urlForecast + location + currentWeatherQuery + "&timezone=" + timezone))
      json <- getJson(response)
      weatherResponse <- decode[WeatherResponse](json).toTry
    yield
      s"$city Conditions: $weatherResponse"

@main def whatAreTheBostonConditions1(): Unit =
  Forecast1.getConditions("latitude=42.36&longitude=-71.06", "America/New_York", s"Boston") match
    case Success(time) => println(time)
    case Failure(e)    => System.err.println(s"Failure: ${e.getLocalizedMessage}")