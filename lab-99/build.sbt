name := "lab-99"

version := "1.0"

scalaVersion := "2.13.1"

Compile / scalacOptions ++= Seq("-Vimplicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"

Test / unmanagedSourceDirectories += baseDirectory.value / "src/it/scala"
