organization := "edu.neu.coe.csye7200"

name := "Numerics"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.12.7"

val scalaTestVersion = "3.0.1"

//ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

//EclipseKeys.withSource := true

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
  "org.spire-math" %% "spire" % "0.13.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "org.scalacheck" %% "scalacheck" % "1.13.5" % "test",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)

