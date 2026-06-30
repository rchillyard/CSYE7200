name := "HelloWorld"

version := "1.0"

// Scala 3 — inherits scalacOptions from root build.sbt

libraryDependencies ++= Seq(
  "com.phasmidsoftware" %% "flog"      % Versions.flog,
  "io.circe"      %% "circe-core"      % Versions.circe,
  "io.circe"      %% "circe-generic"   % Versions.circe,
  "io.circe"      %% "circe-parser"    % Versions.circe,
  "com.lihaoyi"   %% "requests"        % Versions.requests,
  "ch.qos.logback" % "logback-classic" % Versions.logback % Runtime,
  "org.scalatest" %% "scalatest"       % Versions.scalatest % Test
)
