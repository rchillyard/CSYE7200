name := "HelloWorld Part Two"

version := "1.0"

Compile / doc / scalacOptions ++= Seq("-implicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
libraryDependencies += "com.lihaoyi" %% "requests" % "0.8.0"

val sprayGroup = "io.spray"
val sprayJsonVersion = "1.3.6"
libraryDependencies ++= List("spray-json") map { c => sprayGroup %% c % sprayJsonVersion }