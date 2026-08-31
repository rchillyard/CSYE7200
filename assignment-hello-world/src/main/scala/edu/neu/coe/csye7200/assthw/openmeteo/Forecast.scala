package edu.neu.coe.csye7200.assthw.openmeteo

import edu.neu.coe.csye7200.assthw.Tries.{tryEquals, tryNotEquals}
import edu.neu.coe.csye7200.assthw.openmeteo.WeatherResponse.{currentWeatherQuery, urlForecast}
import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*
import scala.util.{Failure, Success, Try}

case class CurrentWeather(temperature: Double, time: String, windspeed: Double, winddirection: Double):
  override def toString: String =
  s"""
     |  local time: $time
     |  temperature: $temperature°C
     |  wind: $windspeed km/h @ $winddirection°""".stripMargin

case class WeatherResponse(latitude: Double, longitude: Double, current_weather: CurrentWeather):
  override def toString: String =
    s"($latitude, $longitude): $current_weather"

object WeatherResponse:
  val urlForecast = "https://api.open-meteo.com/v1/forecast?"
  val currentWeatherQuery = "&current_weather=true"

object Forecast0:
  def showConditions(location: String, city: String): Unit =
    val response = requests.get(urlForecast + location + currentWeatherQuery)
    if (response.statusCode == 200)
    then
      if (response.headers("content-type").contains("application/json; charset=utf-8"))
      then println(s"$city Conditions: ${response.text()}")
      else System.err.println(s"""bad content type: ${response.headers("content-type")}""")
    else System.err.println(s"Bad status code: ${response.statusCode}")

@main def whatsTheTime0(): Unit =
  Forecast0.showConditions("latitude=42.36&longitude=-71.06", s"Boston")

object Forecast1:
  def getConditions(location: String, city: String): Try[String] =
    for
      response <- Try(requests.get(urlForecast + location + currentWeatherQuery))
      _ <- tryEquals(response.statusCode, 200, "invalid status")
      _ <- tryEquals(response.headers("content-type"), List("application/json; charset=utf-8"), "bad content type")
      body = response.text()
      json <- tryNotEquals(body, "", "empty json")
      weatherResponse <- decode[WeatherResponse](body).toTry
    yield
      s"$city Conditions: $weatherResponse"

@main def whatsTheTimeB(): Unit =
  Forecast1.getConditions("latitude=42.36&longitude=-71.06", s"Boston") match
    case Success(time) => println(time)
    case Failure(e)    => System.err.println(s"Failure: ${e.getLocalizedMessage}")