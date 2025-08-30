name := """Concordance"""

version := "1.0"

// Using scalaVersion "2.13" (if you want to change it, go to build.sbt at the root of the project)

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

Compile / doc / scalacOptions ++= Seq("-implicits", "-deprecation")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0" withSources()
)