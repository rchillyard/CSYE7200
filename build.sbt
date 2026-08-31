name := "CSYE7200"

version := "1.1" // Added `core` module.

ThisBuild / scalaVersion := Versions.scala3

// ── Compiler options ─────────────────────────────────────────────────────
// Scala 3 defaults for all modules that inherit the root scalaVersion.
// Modules pinned to Scala 2.12 or 2.13 override this in their own build.sbt.
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wunused:all",
  "-Wvalue-discard"
)

// ── JDK settings ─────────────────────────────────────────────────────────
// JDK 21 (LTS) is the recommended JDK for this project.
// Hadoop/Spark do not yet support JDK 23+ (Subject.getSubject() was removed).
// Set environment variable JDK21_HOME to your JDK 21 installation, e.g.:
//   export JDK21_HOME=/path/to/jdk-21
// Note: assignment-spark-word-count overrides javaHome to JDK 11 locally.
ThisBuild / javaHome := sys.env.get("JDK21_HOME").map(file)

ThisBuild / javacOptions ++= Seq("--release", "17")

// Fork JVMs for tests and runs so javaHome is honoured.
ThisBuild / Test / fork := true
ThisBuild / run / fork  := true

// ── JVM memory ───────────────────────────────────────────────────────────
javaOptions ++= Seq("-Xms512M", "-Xmx2048M")

// ── Test settings ────────────────────────────────────────────────────────
Test / parallelExecution := false
Test / testOptions += Tests.Filter(s => s.endsWith("Test") || s.endsWith("Spec"))

// ── Shared test sources ──────────────────────────────────────────────────
// CancelOnNotImplemented lives in shared-test rather than in any one module,
// and is compiled by each module that needs it.
//
// It cannot simply live in `core` and be depended upon. The nine modules which
// dependsOn(core % "test->test") do inherit it from there, but the three which
// carry their own Scala version cannot: ex-functional-programming is pinned to
// 2.13, and lab-parser and assignment-spark-word-count to 2.12, so none of them
// can consume a Scala 3 test artifact. Sharing the source sidesteps that -- each
// module compiles it with its own compiler. Previously there were four identical
// copies, which is what this replaces.
lazy val sharedTestSources = Seq(
  Test / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "shared-test" / "scala"
)

// ── Module definitions ───────────────────────────────────────────────────
lazy val core    = (project in file("core")).settings(sharedTestSources)

lazy val asstfc  = (project in file("assignment-functional-composition")).dependsOn(core % "compile->compile;test->test")
lazy val assthw  = (project in file("assignment-hello-world")).dependsOn(core % "compile->compile;test->test")
lazy val asstll  = (project in file("assignment-lazy")).dependsOn(core % "compile->compile;test->test")
lazy val asstmd  = (project in file("assignment-movie-database")).dependsOn(core % "compile->compile;test->test")
lazy val asstrs  = (project in file("assignment-random-state")).dependsOn(core % "compile->compile;test->test")
lazy val asstwc  = (project in file("assignment-web-crawler")).dependsOn(core % "compile->compile;test->test")
lazy val asstsw  = (project in file("assignment-spark-word-count")).settings(
  scalaVersion := Versions.scala2_12
).settings(sharedTestSources)
lazy val exconc   = (project in file("ex-concordance")).settings(
  scalaVersion := Versions.scala2_13
)
lazy val exfp      = (project in file("ex-functional-programming")).settings(
  scalaVersion := Versions.scala2_13
).settings(sharedTestSources)
lazy val ex99      = (project in file("lab-scala-99")).dependsOn(core % "compile->compile;test->test")
lazy val labparser  = (project in file("lab-parser")).settings(
  // scala-parser-combinators has no artifact for 2.13+, so this remains on 2.12
  scalaVersion := Versions.scala2_12
).settings(sharedTestSources)
lazy val labsort    = (project in file("lab-sorted")).dependsOn(core % "compile->compile;test->test")
lazy val labactors  = (project in file("lab-actors")).dependsOn(core % "compile->compile;test->test")
lazy val exmr       = project in file("ex-map-reduce")
lazy val exnum      = (project in file("ex-numerics")).dependsOn(core % "compile->compile;test->test")
lazy val sparkapp   = (project in file("ex-spark-app")).settings(
  scalaVersion := Versions.scala2_12
)
lazy val sparkcsv   = (project in file("ex-spark-csv")).settings(
  scalaVersion := Versions.scala2_13
)
lazy val sparkparquet = (project in file("ex-spark-parquet")).settings(
  scalaVersion := Versions.scala2_13
)
lazy val exzio = project in file("ex-zio")

// NOTE: lab-akka-streams (Scala 2.11, EOL) has been retired and is no longer
// included in the aggregate build.
//lazy val labakkastreams  = project in file("lab-akka-streams")
// Similarly, excache has been removed at least temporarily.
//lazy val excache  = project in file("ex-cache")

lazy val root = (project in file(".")).aggregate(
  core,
  assthw, asstmd, asstrs, asstfc, asstwc, asstsw,
  exfp, exnum, exmr, exconc, labsort, labparser, labactors,
  ex99, sparkapp, sparkcsv, sparkparquet, exzio,
  asstll
)
