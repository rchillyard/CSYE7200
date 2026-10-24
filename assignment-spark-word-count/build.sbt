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

// Needed on JDK 17+, and harmless on JDK 11 thanks to
// -XX:+IgnoreUnrecognizedVMOptions, so they are set unconditionally:
// JAVA_11_HOME is unset on CI and on most machines. Without these, Spark aborts
// at start-up (StorageUtils cannot reach sun.nio.ch.DirectBuffer) and, once past
// that, Kryo cannot build a serializer for java.lang.invoke.SerializedLambda.
//
// This is the list spark-submit adds for itself on JDK 17+
// (org.apache.spark.launcher.JavaModuleOptions); a forked test JVM gets no such
// help, so we repeat it here. This is the only module whose tests actually start
// a SparkContext -- the three ex-spark-* modules have theirs ignored.
Test / javaOptions ++= Seq(
  "-XX:+IgnoreUnrecognizedVMOptions",
  "--add-opens=java.base/java.lang=ALL-UNNAMED",
  "--add-opens=java.base/java.lang.invoke=ALL-UNNAMED",
  "--add-opens=java.base/java.lang.reflect=ALL-UNNAMED",
  "--add-opens=java.base/java.io=ALL-UNNAMED",
  "--add-opens=java.base/java.net=ALL-UNNAMED",
  "--add-opens=java.base/java.nio=ALL-UNNAMED",
  "--add-opens=java.base/java.util=ALL-UNNAMED",
  "--add-opens=java.base/java.util.concurrent=ALL-UNNAMED",
  "--add-opens=java.base/java.util.concurrent.atomic=ALL-UNNAMED",
  "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED",
  "--add-opens=java.base/sun.nio.cs=ALL-UNNAMED",
  "--add-opens=java.base/sun.security.action=ALL-UNNAMED",
  "--add-opens=java.base/sun.util.calendar=ALL-UNNAMED",
  "--add-opens=java.security.jgss/sun.security.krb5=ALL-UNNAMED"
)
