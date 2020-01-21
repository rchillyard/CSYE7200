name := "lab-99"

version := "1.0"

scalaVersion := "2.13.1"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"

unmanagedSourceDirectories in Test += baseDirectory.value / "src/it/scala"
