name := """Concordance"""

version := "1.0"

scalaVersion := "2.13.10"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

Compile / doc / scalacOptions ++= Seq("-Vimplicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.2" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2" withSources()
)