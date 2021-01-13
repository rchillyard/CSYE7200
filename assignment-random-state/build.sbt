name := "RandomState"

version := "1.0"

scalaVersion := "2.13.4"

scalacOptions in(Compile, doc) ++= Seq("-groups", "-implicits", "-deprecation")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"
