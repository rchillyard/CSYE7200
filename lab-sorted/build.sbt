name := "LabSorted"

version := "1.0"

scalaVersion := "2.13.4"

scalacOptions in(Compile, doc) ++= Seq("-groups", "-implicits", "-deprecation")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"