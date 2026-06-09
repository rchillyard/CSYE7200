name := "core"

version := "1.0"

// Inherits Scala 3 and scalacOptions from root ThisBuild settings.

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % Versions.scalatest % Test
)