package edu.neu.coe.csye7200.asstzio

import edu.neu.coe.csye7200.asstzio.client.TimeClient
import java.time.format.TextStyle
import java.time.{LocalDateTime, ZoneId, ZoneOffset}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.shouldBe

class WorldTimeSpec extends AnyFlatSpec with Matchers {

  behavior of "WorldTime"

  it should "get correct timezone for empty string" in {
    WorldTime.getTZ("") shouldBe ZoneId.systemDefault()
  }

  it should "get correct timezone for specific timezone NY" in {
    ZoneId.of("America/New_York") shouldBe WorldTime.getTZ("America/New_York")
  }

  ignore should "get correct timezone for specific timezone -5" in {
    val UTC_5: ZoneId = ZoneId.of("UTC-5")
    println(UTC_5)
    val NewYork: ZoneId = WorldTime.getTZ("America/New_York")
    NewYork.getDisplayName(TextStyle.FULL, java.util.Locale.US) shouldBe "Eastern Time"
    WorldTime.getTZ("UTC-5") shouldBe UTC_5
    NewYork shouldBe UTC_5
  }

  it should "calculate days in year correctly" in {
    val leapYearTime = WorldTime(
      LocalDateTime.of(2024, 1, 1, 0, 0),
      "UTC",
      ZoneOffset.UTC,
      1,
      1
    )
    val normalYearTime = WorldTime(
      LocalDateTime.of(2023, 1, 1, 0, 0),
      "UTC",
      ZoneOffset.UTC,
      1,
      1
    )

    leapYearTime.daysInYear shouldBe 366
    normalYearTime.daysInYear shouldBe 365
  }

  it should "format toString correctly" in {
    val worldTime = WorldTime(
      LocalDateTime.of(2023, 11, 15, 12, 0),
      "UTC",
      ZoneOffset.UTC,
      319,
      46
    )
    worldTime.toString shouldBe "Datetime for UTC: 2023-11-15T12:00 (Z); 46/52, 319/365"
  }

  behavior of "TimeClient"

  it should "match offsetRegex(UTC)" in {
    "UTC" match {
      case TimeClient.offsetRegex(null, null) =>
        println("UTC")
      case x =>
        fail(s"unexpected match, got $x")
    }
  }

  it should "match offsetRegex(UTC-4)" in {
    "UTC-4" match {
      case TimeClient.offsetRegex(s, h) =>
        s shouldBe "-"
        h shouldBe "4"
      case x =>
        fail(s"unexpected match, got $x")
    }
  }

  it should "match Europe" in {
    "Europe" match {
      case TimeClient.tzRegex(w, _, null) =>
        w shouldBe "Europe"
      case x =>
        fail(s"unexpected match, got $x")
    }
  }

  it should "match Europe/Paris" in {
    "Europe/Paris" match {
      case TimeClient.tzRegex("Europe", _, "Paris") =>
        succeed
      case x =>
        fail(s"unexpected match, got $x")
    }
  }
}