name := "HelloWorld Part Two"

version := "1.0"

// Using scalaVersion "2.13" (if you want to change it, go to build.sbt at the root of the project)

Compile / doc / scalacOptions ++= Seq("-Vimplicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
//libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"
libraryDependencies += "com.lihaoyi" %% "requests" % "0.8.0"
//libraryDependencies += "io.circe" %% "circe-core" % "0.14.14"

val sprayGroup = "io.spray"
val sprayJsonVersion = "1.3.6"
libraryDependencies ++= List("spray-json") map { c => sprayGroup %% c % sprayJsonVersion }