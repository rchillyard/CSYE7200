name := s"""Parser (Scala ${scalaVersion.value})"""
// Inherits Scala 3 and scalacOptions from root ThisBuild settings.

version := "1.0"

libraryDependencies ++= Seq(
  "com.phasmidsoftware"        %% "lascala"                  % Versions.lascala,
  "ch.qos.logback"              % "logback-core"             % Versions.logback % Runtime,
  "ch.qos.logback"              % "logback-classic"          % Versions.logback % Runtime,
  "org.slf4j"                   % "slf4j-api"                % Versions.slf4j,
  "org.scala-lang.modules"     %% "scala-parser-combinators" % Versions.parserCombinators,
  "org.scalatest"              %% "scalatest"                % Versions.scalatest % Test
)
