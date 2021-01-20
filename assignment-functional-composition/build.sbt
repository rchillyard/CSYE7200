name := "FunctionalComposition"

version := "1.0"

scalaVersion := "2.13.4"

scalacOptions in(Compile, doc) ++= Seq("-groups", "-implicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused" )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"

val sprayGroup = "io.spray"
val sprayJsonVersion = "1.3.5"
libraryDependencies ++= List("spray-json") map { c => sprayGroup %% c % sprayJsonVersion }