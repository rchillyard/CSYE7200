package edu.neu.coe.csye7200.asstzio

//object TimeClientSpec extends zio.test.junit.JUnitRunnableSpec {
//
//  def spec: Spec[TestEnvironment & Scope, Any] = {
//    suite("TimeClient")(
//      test("requestResponseJson UTC") {
//        val urlUTC = "https://worldtimeapi.org/api/timezone/UTC"
//        val zioWorldTime = requestResponseJson[WorldTime](urlUTC)
//        assertZIO(zioWorldTime)(Assertion.assertion("should be OK") {
//          utcTime =>
//            utcTime.timezone == "UTC" && utcTime.utc_offset == ZoneOffset.UTC
//        }).provide(Client.default)
//      },
//      test("requestResponseJson America/New_York") {
//        val urlUTC = "https://worldtimeapi.org/api/timezone/America/New_York"
//        val zioWorldTime = requestResponseJson[WorldTime](urlUTC)
//        assertZIO(zioWorldTime)(Assertion.assertion("should be OK") {
//          utcTime =>
//            utcTime.timezone == "America/New_York" && utcTime.utc_offset == ZoneOffset.of("-04:00")
//        }).provide(Client.default)
//      },
//      test("city List Mexico") {
//        val cityList = getCityList("Mexico")
//        assertZIO(cityList)(Assertion.assertion("cityList should be OK") {
//          list =>
//            list.length == 3
//        }).provide(Client.default)
//      }
//    )
//  }
//}
