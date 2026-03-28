name := s"""SparkCsv (Scala ${scalaVersion.value})"""
// Using scalaVersion "2.13" (if you want to change it, go to build.sbt at the root of the project)

version := "1.0"

Compile / doc / scalacOptions ++= Seq("-groups", "-implicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused" )
ThisBuild / javacOptions ++= Seq("--release", "17") // This is redundant


unmanagedBase := baseDirectory.value / "spark-csv/lib"

Test / parallelExecution := false

val TableParserVersion = "1.5.1"
val sparkVersion = "4.0.1"

ThisBuild / evictionErrorLevel := Level.Warn

libraryDependencies ++= Seq(
  "com.phasmidsoftware" %% "tableparser-spark" % TableParserVersion,
  "com.github.nscala-time" %% "nscala-time" % "3.0.0",
  "org.scalatest" %% "scalatest" % "3.2.20" % "test",
  "org.apache.spark" %% "spark-core" % sparkVersion,
  "org.apache.spark" %% "spark-sql" % sparkVersion,
  "org.apache.spark" %% "spark-mllib" % sparkVersion
)