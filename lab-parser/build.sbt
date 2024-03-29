name := "LabParser"

version := "1.0"

// There is no LaScala for 2.13 so this will have to remain at 2.12 for now
scalaVersion := "2.12.17"

Compile / doc / scalacOptions ++= Seq("-Vimplicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused")

libraryDependencies ++= Seq(
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
  "ch.qos.logback" % "logback-core" % "1.4.5",
  "ch.qos.logback" % "logback-classic" % "1.4.5" % "runtime",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.slf4j" % "slf4j-api" % "1.7.25"
)