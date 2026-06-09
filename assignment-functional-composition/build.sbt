name := s"""Functional Composition (Scala ${scalaVersion.value})"""
// Scala 3 — inherits scalacOptions from root build.sbt

version := "1.0"

libraryDependencies ++= Seq(
  "io.circe"      %% "circe-core"    % Versions.circe,
  "io.circe"      %% "circe-generic" % Versions.circe,
  "io.circe"      %% "circe-parser"  % Versions.circe,
  "org.scalatest" %% "scalatest"     % Versions.scalatest % Test
)