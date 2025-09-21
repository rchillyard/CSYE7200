name := s"""FunctionalProgramming (Scala ${scalaVersion.value})"""
// Using scalaVersion "2.13" (if you want to change it, go to build.sbt at the root of the project)

version := "1.0"

Compile / doc / scalacOptions ++= Seq("-implicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused")
scalacOptions ++= Seq("-encoding", "UTF-8")

lazy val scalaModules = "org.scala-lang.modules"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  scalaModules %% "scala-xml" % "2.4.0",
  "io.spray" %%  "spray-json" % "1.3.6",
  "joda-time" % "joda-time" % "2.14.0",
  "junit" % "junit" % "4.13.2" % "test",
  scalaModules %% "scala-parser-combinators" % "2.4.0"
)