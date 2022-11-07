name := "WebCrawler"

version := "1.0"

scalaVersion := "2.13.4"

Compile / doc / scalacOptions ++= Seq("-Vimplicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused")
scalacOptions ++= Seq("-encoding", "UTF-8")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.2" % "test",
  "org.scala-lang.modules" %% "scala-xml" % "1.2.0",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.2",
  "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1"
)