organization := "edu.neu.coe.csye7200"

name := "Numerics"

version := "1.0.0-SNAPSHOT"

Compile / doc / scalacOptions ++= Seq("-Vimplicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused")

val scalaTestVersion = "3.2.15"

libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "2.1.0",
  "org.typelevel" %% "cats-core" % "2.9.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0",
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
  "ch.qos.logback" % "logback-core" % "1.4.5",
  "ch.qos.logback" % "logback-classic" % "1.4.5" % "runtime",
  "org.scalatest" %% "scalatest" % "3.2.15" % "test",
  "org.scalacheck" %% "scalacheck" % "1.17.0" % "test"
)
