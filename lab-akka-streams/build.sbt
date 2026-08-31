name := "Akka Streams (Scala 2.11)"
organization    := "com.phasmidsoftware"
scalaVersion    := "2.11.12"
version :="0.1.0-SNAPSHOT"

Compile / doc / scalacOptions ++= Seq("-Vimplicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused")

val akkaGroup = "com.typesafe.akka"
val sprayGroup = "io.spray"
val sprayVersion = "1.3.4"
val sprayJsonVersion = "1.3.4"
val scalaTestVersion = "3.0.5"

lazy val akkaHttpVersion = "10.1.8"
lazy val akkaVersion    = Versions.akka

libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-http"            % akkaHttpVersion,
      "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion,
      "com.typesafe.akka" %% "akka-http-xml"        % akkaHttpVersion,
      "com.typesafe.akka" %% "akka-stream"          % Versions.akka,
      "com.typesafe.akka" %% "akka-actor"           % Versions.akka,
      "com.typesafe.akka" %% "akka-slf4j"           % Versions.akka,
      "ch.qos.logback"    % "logback-classic"       % "1.4.5",
      "com.typesafe.scala-logging" %% "scala-logging" %   Versions.scalaLogging,
      "org.json4s"        %% "json4s-jackson"       % "3.6.7",
      "org.scalaj"        %% "scalaj-http"          % "2.4.2",
      "com.typesafe.akka" %% "akka-http-testkit"    % akkaHttpVersion % Test,
      "com.typesafe.akka" %% "akka-testkit"         % Versions.akka     % Test,
      "com.typesafe.akka" %% "akka-stream-testkit"  % Versions.akka     % Test,
      "org.scalatest"     %% "scalatest"            % "3.0.5"         % Test
)