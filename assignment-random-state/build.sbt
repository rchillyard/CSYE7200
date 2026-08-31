name := "RandomState"

version := "1.0"

// Scala 3 — inherits scalacOptions from root build.sbt
// Note: scalactic is a transitive dependency of scalatest and does not need
// to be listed separately.

libraryDependencies += "org.scalatest" %% "scalatest" % Versions.scalatest % Test
