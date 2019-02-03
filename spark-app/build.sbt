name := """SparkApp"""

version := "1.0"

scalaVersion := "2.11.9"

parallelExecution in Test := false

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.apache.spark" % "spark-core_2.11" % "2.2.0",
  "org.apache.spark" % "spark-sql_2.11" % "2.2.0",
  "org.apache.spark" % "spark-mllib_2.11" % "2.2.0"
)

parallelExecution in Test := false