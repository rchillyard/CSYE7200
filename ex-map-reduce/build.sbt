name := "MapReduce"

version := "1.0.0-SNAPSHOT"

// Scala 3 — inherits scalacOptions from root build.sbt
// Note: spray-json has no Scala 3 artifact. It has been removed pending
// investigation of whether it is actually used. Replace with circe if needed.
// Note: akka-actor (classic API) retained; consider migrating to
// akka-actor-typed (as used in lab-actors) in a future semester.

val akkaGroup        = "com.typesafe.akka"
val akkaVersion      = Versions.akka
val scalaTestVersion = Versions.scalatest

libraryDependencies ++= Seq(
  akkaGroup              %% "akka-actor"    % Versions.akka,
  akkaGroup              %% "akka-testkit"  % Versions.akka  % Test,
  akkaGroup              %% "akka-slf4j"    % Versions.akka,
  "com.typesafe"          % "config"        % "1.4.3",
  "ch.qos.logback"        % "logback-classic" % Versions.logback % Runtime,
  "org.scala-lang.modules" %% "scala-xml"   %   Versions.xml,
  "org.ccil.cowan.tagsoup" % "tagsoup"      % "1.2.1"      % Test,
  "org.scalatest"        %% "scalatest"     % scalaTestVersion % Test
)
