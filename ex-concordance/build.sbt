name := s"""Concordance (Scala ${scalaVersion.value})"""
// Scala 2.13 — see root build.sbt

version := "1.0"

// Scala 2.13 compiler options (override ThisBuild Scala 3 defaults)
scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Ywarn-value-discard",
  "-Ywarn-dead-code"
)

// Note: Typesafe resolver removed — all dependencies are available on Maven Central.

libraryDependencies ++= Seq(
  "org.scalatest"          %% "scalatest"                % Versions.scalatest % Test,
  "org.scala-lang.modules" %% "scala-parser-combinators" % Versions.parserCombinators
)
