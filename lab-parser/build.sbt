name := "LabParser"

version := "1.0"

scalaVersion := "2.11.9"

val spark = "org.apache.spark"
val sparkVersion = "1.6.1"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
//  "com.phasmid" %% "lascala" % "1.0.7",
  spark %% "spark-core" % sparkVersion % "provided",
  spark %% "spark-mllib" % sparkVersion % "provided",
  spark %% "spark-hive" % sparkVersion % "provided",
  spark %% "spark-graphx" % sparkVersion % "provided"
)
