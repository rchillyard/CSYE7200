// project/Dependencies.scala
// Single source of truth for all library versions across the CSYE7200 build.
// Files under project/ are compiled before any build.sbt, so these values
// are available in every subproject's build.sbt without any imports.

object Versions {
  // Scala
  val scala3    = "3.3.6"
  val scala2_13 = "2.13.16"
  val scala2_12 = "2.12.20"

  // Scala libraries
//  val scalaLogging = "3.9.6"
  val parserCombinators = "2.4.0"
  val xml = "2.4.0"

  // Akka
  // NOTE that this version is under BSL but OK for academic use.
  // To avoid the BSL entirely, the community fork Apache Pekko is a drop-in replacement under Apache 2.0
  val akka = "2.8.8"

  // Spark
  // ex-spark-csv and ex-spark-parquet use Spark 4.x (requires Scala 2.13+).
  // assignment-spark-word-count and ex-spark-app stay on Spark 3.5.x + Scala 2.12
  // until an explicit upgrade decision is made.
  val spark3 = "3.5.7"
  val spark4 = "4.0.1"

  // Testing
  val scalatest = "3.2.20"

  // ZIO
  val zio     = "2.1.26"
  val zioHttp = "3.11.2"

  // TableParser (Phasmid Software)
  val tableParser2 = "1.5.1"

  // Circe (JSON)
  val circe = "0.14.15"

  // Misc
  val nscalaTime = "3.0.0"
  val jodaTime   = "2.14.2"
  val osLib      = "0.11.4"
  val requests   = "0.9.3"
  val upickle    = "4.1.0"
  val slf4j      = "2.0.18"
  val logback    = "1.5.34"
}