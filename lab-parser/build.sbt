name := "LabParser"

version := "1.0"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
//  "com.phasmid" %% "lascala" % "1.0.7",
)
