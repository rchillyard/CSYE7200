name := "WebCrawler"

version := "1.0"

scalaVersion := "2.12.10"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
  "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1"
)