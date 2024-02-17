name := """SparkCsv"""

version := "1.0"

Compile / doc / scalacOptions ++= Seq("-groups", "-implicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused" )

unmanagedBase := baseDirectory.value / "spark-csv/lib"

Test / parallelExecution := false

val sparkVersion = "3.2.1"

libraryDependencies ++= Seq(
  "com.phasmidsoftware" %% "tableparser" % "1.0.14",
  "com.github.nscala-time" %% "nscala-time" % "2.24.0",
  "org.scalatest" %% "scalatest" % "3.2.3" % "test",
  "org.apache.spark" %% "spark-core" % sparkVersion,
  "org.apache.spark" %% "spark-sql" % sparkVersion,
  "org.apache.spark" %% "spark-mllib" % sparkVersion
)