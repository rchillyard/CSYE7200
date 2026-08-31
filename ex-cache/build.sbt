name := "Cache"

version := "1.0"

// Scala 3 — inherits scalacOptions from root build.sbt
// Note: spray-json has no Scala 3 artifact. It has been removed pending
// investigation of whether it is actually used. Replace with circe if needed.
// Note: junit retained as some tests may depend on it directly.

lazy val scalaModules        = "org.scala-lang.modules"

libraryDependencies ++= Seq(
  "org.scalatest"          %% "scalatest"                % Versions.scalatest  % Test,
  "junit"                   % "junit"                    % "4.13.2"  % Test,
  scalaModules             %% "scala-xml"                % Versions.xml,
  scalaModules             %% "scala-parser-combinators" % Versions.parserCombinators,
  "joda-time"               % "joda-time"               % Versions.jodaTime
)
