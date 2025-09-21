name := s"""FunctionalComposition (Scala ${scalaVersion.value})"""
// Using scalaVersion "2.13" (if you want to change it, go to build.sbt at the root of the project)

version := "1.0"

Compile / doc / scalacOptions ++= Seq("-implicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

val sprayGroup = "io.spray"
val sprayJsonVersion = "1.3.5"
libraryDependencies ++= List("spray-json") map { c => sprayGroup %% c % sprayJsonVersion }