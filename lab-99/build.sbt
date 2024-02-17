name := "lab-99"

version := "1.0"

Compile / scalacOptions ++= Seq("-Vimplicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.2"
libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.9.0"
libraryDependencies += "com.lihaoyi" %% "requests" % "0.8.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"
libraryDependencies +=  "com.lihaoyi" %% "upickle" % "3.0.0-M1" //"com.lihaoyi" %% "ujson" % "0.6.5"

Test / unmanagedSourceDirectories += baseDirectory.value / "src/it/scala"