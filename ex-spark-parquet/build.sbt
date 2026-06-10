name := s"""SparkParquet (Scala ${scalaVersion.value})"""
// Scala 2.13 — see root build.sbt

version := "1.0"

// Scala 2.13 compiler options (override ThisBuild Scala 3 defaults)
Compile / scalacOptions := Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Ywarn-unused",
  "-Ywarn-value-discard",
  "-Ywarn-dead-code"
)

// lib/ directory for any unmanaged jars
unmanagedBase := baseDirectory.value / "lib"

Test / parallelExecution := false
ThisBuild / evictionErrorLevel := Level.Warn

// Exclude log4j-slf4j2-impl from Spark to avoid multiple SLF4J provider warnings
// (logback is used instead, via the root build)
excludeDependencies ++= Seq(
  ExclusionRule("org.apache.logging.log4j", "log4j-slf4j2-impl")
)

libraryDependencies ++= Seq(
  "com.phasmidsoftware"    %% "tableparser-spark"   % Versions.tableParser2,
  "com.phasmidsoftware"    %% "tableparser-parquet" % Versions.tableParser2,
  "com.github.nscala-time" %% "nscala-time"         % Versions.nscalaTime,
  "org.scalatest"          %% "scalatest"            % Versions.scalatest % Test,
  "org.apache.spark"       %% "spark-core"           % Versions.spark4,
  "org.apache.spark"       %% "spark-sql"            % Versions.spark4,
  "org.apache.spark"       %% "spark-mllib"          % Versions.spark4
)