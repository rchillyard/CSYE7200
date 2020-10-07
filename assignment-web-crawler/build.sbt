name := "WebCrawler"

version := "1.0"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.1.0" % "test",
  "org.scala-lang.modules" %% "scala-xml" % "1.2.0",
  "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1"
)