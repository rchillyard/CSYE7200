name := "LabActors"

version := "1.0"

scalaVersion := "2.11.9"

val akkaGroup = "com.typesafe.akka"
val akkaVersion = "2.5.17"
val sprayGroup = "io.spray"
val sprayVersion = "1.3.4"
val sprayJsonVersion = "1.3.4"
val scalaTestVersion = "3.0.5"

libraryDependencies ++= List("spray-client") map {c => sprayGroup %% c % sprayVersion}
libraryDependencies ++= List("spray-json") map {c => sprayGroup %% c % sprayJsonVersion}

libraryDependencies ++= Seq(
  akkaGroup %% "akka-actor" % akkaVersion,
  akkaGroup %% "akka-testkit" % akkaVersion % "test",
  akkaGroup %% "akka-slf4j" % akkaVersion,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "com.typesafe" % "config" % "1.3.2",
  "com.github.nscala-time" %% "nscala-time" % "2.18.0",
  "ch.qos.logback" % "logback-core" % "1.2.3",
  "ch.qos.logback" % "logback-classic" % "1.2.3" % "runtime",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)