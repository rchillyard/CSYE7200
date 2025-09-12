package edu.neu.coe.csye7200.asstzio.client.specs

import edu.neu.coe.csye7200.asstzio.WorldTime
import edu.neu.coe.csye7200.asstzio.client.TimeClient.requestResponseJson
import edu.neu.coe.csye7200.asstzio.client.TimeClientException
import java.time.ZoneOffset
import zio.*
import zio.http.*
import zio.test.*

object TimeClientSpec extends ZIOSpecDefault {

  def spec: Spec[TestEnvironment & Scope, Any] = test("requestResponseJson") {

    val zioWorldTime: ZIO[http.Client, TimeClientException, WorldTime] =
      requestResponseJson[WorldTime]("https://worldtimeapi.org/api/timezone/UTC")
    assertZIO(zioWorldTime.provide(Client.default))(Assertion.assertion("should be OK") {
      utcTime =>
        utcTime.timezone == "UTC" && utcTime.utc_offset == ZoneOffset.UTC
    })
  }
}