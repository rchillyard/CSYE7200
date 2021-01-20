name := "FunctionalProgramming"

version := "1.0"

scalaVersion := "2.13.4"

scalacOptions in(Compile, doc) ++= Seq("-groups", "-implicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused" )
scalacOptions ++= Seq( "-encoding", "UTF-8")


lazy val scalaModules = "org.scala-lang.modules"

libraryDependencies ++= Seq(
//  "com.phasmidsoftware" %% "tableparser" % "1.0.8",
  "org.scalatest" %% "scalatest" % "3.2.2" % "test",
  scalaModules %% "scala-xml" % "1.3.0",
  "io.spray" %%  "spray-json" % "1.3.6",
  "joda-time" % "joda-time" % "2.9.9",
  "junit" % "junit" % "4.12" % "test",
  scalaModules %% "scala-parser-combinators" % "1.1.2"
)