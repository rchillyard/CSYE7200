name := s"""FunctionalProgramming (Scala ${scalaVersion.value})"""
// Scala 2.13 — see root build.sbt

version := "1.0"

// Scala 2.13 compiler options (override ThisBuild Scala 3 defaults)
scalacOptions := Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-encoding", "UTF-8",
  "-Ywarn-unused",
  "-Ywarn-value-discard",
  "-Ywarn-dead-code"
)

lazy val scalaModules = "org.scala-lang.modules"

libraryDependencies ++= Seq(
  "com.phasmidsoftware"    %% "tableparser-core"         % Versions.tableParser2,
  scalaModules             %% "scala-xml"                % Versions.xml,
  scalaModules             %% "scala-parser-combinators" % Versions.parserCombinators,
  "io.circe"               %% "circe-core"               % Versions.circe,
  "io.circe"               %% "circe-generic"            % Versions.circe,
  "io.circe"               %% "circe-parser"             % Versions.circe,
  "joda-time"               % "joda-time"               % Versions.jodaTime,
  "org.scalatest"          %% "scalatest"                % Versions.scalatest  % Test,
  "junit"                   % "junit"                    % "4.13.2"  % Test
)
