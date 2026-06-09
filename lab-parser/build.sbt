name := s"""Parser (Scala ${scalaVersion.value})"""
// Scala 2.12 — see root build.sbt
// Note: retaining 2.12 as there is no LaScala for 2.13+.

version := "1.0"

// Scala 2.12 compiler options (override ThisBuild Scala 3 defaults)
scalacOptions := Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Ywarn-unused",
  "-Ywarn-value-discard",
  "-Ywarn-dead-code"
)

libraryDependencies ++= Seq(
//  "com.typesafe.scala-logging" %% "scala-logging"            % Versions.scalaLogging,
  "ch.qos.logback"              % "logback-core"             % Versions.logback % Runtime,
  "ch.qos.logback"              % "logback-classic"          % Versions.logback % Runtime,
  "org.slf4j"                   % "slf4j-api"                % Versions.slf4j,
  "org.scala-lang.modules"     %% "scala-parser-combinators" % Versions.parserCombinators,
  "org.scalatest"              %% "scalatest"                % Versions.scalatest % Test
)