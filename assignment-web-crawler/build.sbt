name := "WebCrawler"

version := "1.0"

// Scala 3 — inherits scalacOptions from root build.sbt
// Note: tagsoup (1.2.1) is unmaintained but retained as no drop-in
// Scala 3 replacement exists. Consider replacing with jsoup if this
// module is actively developed.

scalacOptions ++= Seq("-encoding", "UTF-8")

libraryDependencies ++= Seq(
  "org.scalatest"            %% "scalatest"                  % Versions.scalatest % Test,
  "org.scala-lang.modules"   %% "scala-xml"                  % Versions.xml,
  "org.scala-lang.modules"   %% "scala-parallel-collections" % "1.2.0",
  "org.ccil.cowan.tagsoup"    % "tagsoup"                    % "1.2.1"
)