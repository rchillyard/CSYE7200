name := "MapReduce"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.13.4"

scalacOptions in(Compile, doc) ++= Seq("-groups", "-implicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused" )

//mainClass in (Compile, run) := Some("edu.neu.coe.scala.mapreduce.MapReduce")

val akkaGroup = "com.typesafe.akka"
val akkaVersion = "2.6.10"
val sprayGroup = "io.spray"
val sprayVersion = "1.3.3"
val sprayJsonVersion = "1.3.6"
val scalaTestVersion = "3.2.2"

//ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

//libraryDependencies ++= List("spray-client") map {c => sprayGroup %% c % sprayVersion}
libraryDependencies ++= List("spray-json") map { c => sprayGroup %% c % sprayJsonVersion }

//libraryDependencies += compilerPlugin("com.typesafe" %% "abide" % "0.1-SNAPSHOT")

//scalacOptions ++= Seq(
// "-P:abide:abidecp:<some.rules.classpath>",
//  "-P:abide:ruleClass:<some.rule.Class>",
//  "-P:abide:analyzerClass:<some.analyzer.generator.Module>",
//  "-P:abide:presenterClass:<some.presenter.generator.Module>")

//libraryDependencies += "com.typesafe" %% "abide-core" % "0.1-SNAPSHOT" % "abide"

libraryDependencies ++= Seq(
  akkaGroup %% "akka-actor" % akkaVersion,
  akkaGroup %% "akka-testkit" % akkaVersion % "test",
  akkaGroup %% "akka-slf4j" % akkaVersion,
  "com.typesafe" % "config" % "1.4.0",
  //	"com.github.nscala-time" %% "nscala-time" % "2.0.0",
  "ch.qos.logback" % "logback-classic" % "1.0.0" % "runtime",
  // xml and tagsoup are for WebCrawler example
  "org.scala-lang.modules" %% "scala-xml" % "1.3.0",
  "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1" % "test",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)
