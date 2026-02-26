name := s"""Spark Word Count (Scala ${scalaVersion.value})"""
// Using scalaVersion "2.12" (if you want to change it, go to build.sbt at the root of the project)

version := "1.0"

// Spark 3.5.x pairs well with Scala 2.12.x and JDK 17
scalaVersion := "2.12.18"

val scalaTestVersion = "3.2.19"
val sparkVersion = "3.5.7"
//val sparkVersion = "4.0.1"

// Set JAVA_HOME to JDK 11 because Spark 3.5.x requires JDK 11 (even though it claims it works with JDK 17)
javaHome := sys.env.get("JAVA_11_HOME").map(file)

libraryDependencies ++= Seq(
  "org.apache.spark" %% "spark-core" % sparkVersion,
  "org.apache.spark" %% "spark-sql" % sparkVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test
)

// Ensure tests fork a JVM (so we control the JDK used)
Test / fork := true

// If you must run on JDK 17+, these are typically not needed with Spark 3.5.x,
// but if your environment is restrictive, you can uncomment these:
 Test / javaOptions ++= Seq(
   "--add-opens=java.base/java.nio=ALL-UNNAMED",
   "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED",
   "--add-exports=java.base/jdk.internal.misc=ALL-UNNAMED"
 )