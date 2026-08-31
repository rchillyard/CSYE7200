organization := "edu.neu.coe.csye7200"

name := "Numerics"

version := "1.0.0-SNAPSHOT"

// Scala 3 — inherits scalacOptions from root build.sbt
// Note: Typesafe resolver removed — all dependencies are on Maven Central.
// Note: duplicate scalatest entry (3.2.15) removed; using 3.2.19 throughout.
// Note: scala-parser-combinators updated from 2.2.0 to 2.4.0.
// Note: logback updated from 1.4.5 to 1.5.18.

libraryDependencies ++= Seq(
  "org.scala-lang.modules"     %% "scala-xml"                % Versions.xml,
  "org.scala-lang.modules"     %% "scala-parser-combinators" % Versions.parserCombinators,
  "org.typelevel"              %% "cats-core"                % "2.13.0",
  "org.apache.commons"          % "commons-math3"            % "3.6.1",
  "org.slf4j" % "slf4j-api" % Versions.slf4j,
  "ch.qos.logback"              % "logback-core"             % Versions.logback % Runtime,
  "ch.qos.logback"              % "logback-classic"          % Versions.logback % Runtime,
  "org.scalatest"              %% "scalatest"                % Versions.scalatest  % Test,
  "org.scalacheck"             %% "scalacheck"               % "1.19.0"  % Test
)
