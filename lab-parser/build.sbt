name := "LabParser"

version := "1.0"

// Using scalaVersion "2.12" (if you want to change it, go to build.sbt at the root of the project)

Compile / doc / scalacOptions ++= Seq("-Vimplicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused")

libraryDependencies ++= Seq(
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
  "ch.qos.logback" % "logback-core" % "1.5.18",
  "ch.qos.logback" % "logback-classic" % "1.5.18" % "runtime",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
  "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  "org.slf4j" % "slf4j-api" % "1.7.25"
)