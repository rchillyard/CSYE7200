name := s"""SparkCsv (Scala ${scalaVersion.value})"""
// Using scalaVersion "2.13" (if you want to change it, go to build.sbt at the root of the project)

version := "1.0"

Compile / doc / scalacOptions ++= Seq("-groups", "-implicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused" )

unmanagedBase := baseDirectory.value / "spark-csv/lib"

Test / parallelExecution := false

val sparkVersion = "3.3.0"

libraryDependencies ++= Seq(
  "com.phasmidsoftware" %% "tableparser" % "1.1.1",
  "com.github.nscala-time" %% "nscala-time" % "2.32.0",
  "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  "org.apache.spark" %% "spark-core" % sparkVersion,
  "org.apache.spark" %% "spark-sql" % sparkVersion,
  "org.apache.spark" %% "spark-mllib" % sparkVersion
)