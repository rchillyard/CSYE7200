name := "SparkWordCount"

version := "0.1"

scalaVersion := "2.12.12"

val scalaTestVersion = "3.2.3"

libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test"

libraryDependencies += "org.apache.spark" %% "spark-core" % "3.0.1"

libraryDependencies += "org.apache.spark" %% "spark-sql" % "3.0.1"

parallelExecution in Test := false