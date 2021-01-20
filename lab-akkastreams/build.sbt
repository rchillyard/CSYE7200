name := "lab-akka-streams"
organization    := "com.phasmidsoftware"
scalaVersion    := "2.11.9"
version :="0.1.0-SNAPSHOT"

scalacOptions in(Compile, doc) ++= Seq("-groups", "-implicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused" )

val akkaGroup = "com.typesafe.akka"
val sprayGroup = "io.spray"
val sprayVersion = "1.3.4"
val sprayJsonVersion = "1.3.4"
val scalaTestVersion = "3.0.5"

lazy val akkaHttpVersion = "10.1.8"
lazy val akkaVersion    = "2.5.23"

libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-http"            % akkaHttpVersion,
      "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion,
      "com.typesafe.akka" %% "akka-http-xml"        % akkaHttpVersion,
      "com.typesafe.akka" %% "akka-stream"          % akkaVersion,
      "com.typesafe.akka" %% "akka-actor"           % akkaVersion,
      "com.typesafe.akka" %% "akka-slf4j"           % akkaVersion,
      "ch.qos.logback"    % "logback-classic"       % "1.0.9",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
      "org.json4s"        %% "json4s-jackson"       % "3.6.7",
      "org.scalaj"        %% "scalaj-http"          % "2.4.2",
      "com.typesafe.akka" %% "akka-http-testkit"    % akkaHttpVersion % Test,
      "com.typesafe.akka" %% "akka-testkit"         % akkaVersion     % Test,
      "com.typesafe.akka" %% "akka-stream-testkit"  % akkaVersion     % Test,
      "org.scalatest"     %% "scalatest"            % "3.0.5"         % Test
)