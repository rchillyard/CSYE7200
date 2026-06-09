name := s"""SparkWordCount (Scala ${scalaVersion.value})"""
// Scala 2.12 — see root build.sbt
// Conservative: staying on Spark 3.5.7 + Scala 2.12 for now.
// Upgrade to Spark 4.x + Scala 2.13 to be considered in a future semester.

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

// Spark 3.5.x works best with JDK 11. Set JAVA_11_HOME to your JDK 11
// installation, e.g.:  export JAVA_11_HOME=/path/to/jdk-11
// If JDK 11 is not available, Spark 3.5.x can run on JDK 17 with the
// JVM options below uncommented.
javaHome := sys.env.get("JAVA_11_HOME").map(file)

libraryDependencies ++= Seq(
  "org.apache.spark" %% "spark-core" % Versions.spark3,
  "org.apache.spark" %% "spark-sql"  % Versions.spark3,
  "org.scalatest"    %% "scalatest"  % Versions.scalatest % Test
)

Test / fork := true

// Uncomment if running on JDK 17+ instead of JDK 11:
// Test / javaOptions ++= Seq(
//   "--add-opens=java.base/java.nio=ALL-UNNAMED",
//   "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED",
//   "--add-exports=java.base/jdk.internal.misc=ALL-UNNAMED"
// )