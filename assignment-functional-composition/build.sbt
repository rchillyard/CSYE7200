name := "Functional Composition"
// Scala 3 — inherits scalacOptions from root build.sbt

version := "1.0"

libraryDependencies ++= Seq(
  "com.phasmidsoftware" % "tableparser-core_2.13" % Versions.tableParser2, // Needed for FP object.
  "io.circe"            %% "circe-core"       % Versions.circe,
  "io.circe"            %% "circe-generic"    % Versions.circe,
  "io.circe"            %% "circe-parser"     % Versions.circe,
  "org.scalatest"       %% "scalatest"        % Versions.scalatest % Test
)
