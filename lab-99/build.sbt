name := "lab-99"

version := "1.0"

scalaVersion := "2.12.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

unmanagedSourceDirectories in Test += baseDirectory.value / "src/it/scala"
