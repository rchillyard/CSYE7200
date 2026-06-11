name := s"""SparkApp (Scala ${scalaVersion.value})"""
// Scala 2.12 — see root build.sbt

version := "1.0"

// Scala 2.12 compiler options (override ThisBuild Scala 3 defaults)
scalacOptions := Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Ywarn-unused",
  "-Ywarn-value-discard",
  "-Ywarn-dead-code"
)

libraryDependencies ++= Seq(
  "org.scalatest"    %% "scalatest"   % Versions.scalatest % Test,
  "org.apache.spark" %% "spark-core"  % Versions.spark3,
  "org.apache.spark" %% "spark-sql"   % Versions.spark3,
  "org.apache.spark" %% "spark-mllib" % Versions.spark3
)

Test / parallelExecution := false
