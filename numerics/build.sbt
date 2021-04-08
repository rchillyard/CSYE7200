organization := "edu.neu.coe.csye7200"

name := "Numerics"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.13.1"

scalacOptions in(Compile, doc) ++= Seq("-groups", "-implicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused" )

val scalaTestVersion = "3.1.1"

libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.2.0",
  //  "org.spire-math" %% "spire" % "0.13.0",
  "org.typelevel" %% "cats-core" % "2.2.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"
//  "org.scalatestplus" %% "scalacheck-1-15" % "3.2.2.0" % "test"
)

