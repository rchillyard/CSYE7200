name := "CSYE7200"

version := "1.0"

ThisBuild / scalaVersion := "3.3.6"

lazy val scalaVersion2_13 = "2.13.16"

lazy val scalaVersion2_12 = "2.12.20"

lazy val asstfc = (project in file("assignment-functional-composition")).settings(
  scalaVersion := scalaVersion2_13
)

lazy val assthw = project in file("assignment-helloworld")

lazy val assthw2 = project in file("assignment-helloworld-2")

lazy val asstll = project in file("assignment-lazy")

lazy val asstmd = project in file("assignment-movie-database")

lazy val asstrs = project in file("assignment-random-state")

lazy val asstwc = project in file("assignment-web-crawler")

lazy val asstsw = (project in file("assignment-spark-wordcount")).settings(
  scalaVersion := scalaVersion2_12
)

lazy val asstch = project in file("asst-cache")

lazy val concor = (project in file("concordance")).settings(
  scalaVersion := scalaVersion2_13)

lazy val fp = (project in file("functional-programming")).settings(
  scalaVersion := scalaVersion2_13)

lazy val lab99 = project in file("lab-99")

lazy val labparser = (project in file("lab-parser")).settings(
   // There is no LaScala for 2.13 so this will have to remain at 2.12 for now
  scalaVersion := scalaVersion2_12)

lazy val labsort = project in file("lab-sorted")

lazy val labactors = project in file("lab-actors")

lazy val mapred = project in file("mapreduce")

lazy val num = project in file("numerics")

lazy val sparkapp = (project in file("spark-app")).settings(
  scalaVersion := scalaVersion2_12
)

lazy val sparkcsv = (project in file("spark-csv")).settings(
  scalaVersion := scalaVersion2_13
)

lazy val sparkparquet = (project in file("spark-parquet")).settings(
  scalaVersion := scalaVersion2_13
)

lazy val zio = project in file("zio")

lazy val root = (project in file(".")).aggregate(assthw, assthw2, asstmd, asstrs, asstfc, asstwc, asstsw, fp, num,  mapred, concor, labsort, labparser, labactors, lab99, asstch, sparkapp, sparkcsv, zio)

Test / parallelExecution := false

javaOptions ++= Seq("-Xms512M", "-Xmx2048M", "-XX:+CMSClassUnloadingEnabled")


// Use JDK 17 for all forked JVMs (tests/runs) across all subprojects.
// Set environment variable JDK17_HOME to your JDK 17 installation, e.g.:
//   export JDK17_HOME=/path/to/jdk-17
ThisBuild / javaHome := sys.env.get("JDK17_HOME").map(file)

// Make sure forked JVMs are used so javaHome is honored.
ThisBuild / Test / fork := true
ThisBuild / run / fork := true

// If you have any Java sources, compile them against Java 17 APIs.
ThisBuild / javacOptions ++= Seq("--release", "17")

Test / testOptions += Tests.Filter(name => name.endsWith("Test") || name.endsWith("Spec"))