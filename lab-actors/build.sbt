name := "Actors"

version := "1.0"

// Scala 3 — inherits scalacOptions from root build.sbt

lazy val scalatestVersion = Versions.scalatest

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed"         % Versions.akka,
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % Versions.akka % Test,
  "ch.qos.logback"     % "logback-classic"           % Versions.logback % Runtime,
  "org.scalatest"     %% "scalatest"                 % scalatestVersion % Test
)