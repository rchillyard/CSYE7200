name := "HelloWorld"

version := "1.0"

// Using scalaVersion "2.13" (if you want to change, go to build.sbt at the root of the project)

Compile / doc / scalacOptions ++= Seq("-implicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused", "-no-indent", "-source:3.0-migration", "-rewrite" )
Compile / doc / scalacOptions ++= Seq(
  "-source:3.0-migration",
  "-Xmigration"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
libraryDependencies += "com.lihaoyi" %% "requests" % "0.9.0"
libraryDependencies += "io.circe" %% "circe-core" % "0.14.14"