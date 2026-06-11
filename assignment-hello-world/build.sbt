name := "HelloWorld"

version := "1.0"

// Scala 3 — inherits scalacOptions from root build.sbt

libraryDependencies ++= Seq(
  "io.circe"      %% "circe-core"    % Versions.circe,
  "io.circe"      %% "circe-generic" % Versions.circe,
  "io.circe"      %% "circe-parser"  % Versions.circe,
  "com.lihaoyi"   %% "requests"      % Versions.requests,
  "org.scalatest" %% "scalatest"     % Versions.scalatest % Test
)
