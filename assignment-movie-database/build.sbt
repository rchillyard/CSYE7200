name := "MovieDatabase"

version := "1.0"

// Scala 3 — inherits scalacOptions from root build.sbt

libraryDependencies ++= Seq(
  "com.phasmidsoftware" % "tableparser-core_2.13" % Versions.tableParser2, // Needed for FP object.
  "org.scalatest"       %% "scalatest"        % Versions.scalatest % Test
)