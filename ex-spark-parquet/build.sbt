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

// JDK 17+ compatibility: Hadoop/Spark uses javax.security.auth.Subject.getSubject()
// which throws UnsupportedOperationException on JDK 18+.
Test / javaOptions ++= Seq(
  "-Djava.security.manager=allow",
  "-Djavax.security.auth.useSubjectCredsOnly=false"
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