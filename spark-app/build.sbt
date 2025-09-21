name := s"""SparkApp (Scala ${scalaVersion.value})"""
// Using scalaVersion "2.12" (if you want to change it, go to build.sbt at the root of the project)

version := "1.0"

Compile / doc / scalacOptions ++= Seq("-implicits", "-deprecation")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  "org.apache.spark" %% "spark-core" % "3.0.1",
  "org.apache.spark" %% "spark-sql" % "3.0.1",
  "org.apache.spark" %% "spark-mllib" % "2.4.0"
)

Test / parallelExecution := false