name := """Concordance"""

version := "1.0"

scalaVersion := "2.12.10"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6" withSources()
)