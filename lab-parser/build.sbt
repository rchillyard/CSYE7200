name := "LabParser"

version := "1.0"

scalaVersion := "2.12.10"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.slf4j" % "slf4j-api" % "1.7.25"
//  "com.phasmid" %% "lascala" % "1.0.7",
)
