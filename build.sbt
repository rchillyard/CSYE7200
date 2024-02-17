name := "CSYE7200"

version := "1.0"

ThisBuild / scalaVersion := "2.13.10"

lazy val scalaVersion2_12 = "2.12.17"

lazy val asstfc = project in file("assignment-functional-composition")

lazy val assthw = project in file("assignment-helloworld")

lazy val asstll = project in file("assignment-lazy")

lazy val asstmd = project in file("assignment-movie-database")

lazy val asstrs = project in file("assignment-random-state")

lazy val asstwc = project in file("assignment-web-crawler")

lazy val asstsw = (project in file("assignment-spark-wordcount")).settings(
  scalaVersion := scalaVersion2_12
)

lazy val asstch = project in file("asst-cache")

lazy val concor = project in file("concordance")

lazy val fp = project in file("functional-programming")

lazy val lab99 = project in file("lab-99")

lazy val labparser = project in file("lab-parser")

lazy val labsort = project in file("lab-sorted")

lazy val labactors = project in file("lab-actors")

lazy val mapred = project in file("mapreduce")

lazy val num = project in file("numerics")

lazy val sparkapp = (project in file("spark-app")).settings(
  scalaVersion := scalaVersion2_12
)

lazy val sparkcsv = (project in file("spark-csv")).settings(
  scalaVersion := scalaVersion2_12
)

lazy val root = (project in file(".")).aggregate(assthw, asstmd, asstrs, asstfc, asstwc, asstsw, fp, num,  mapred, concor, labsort, labparser, labactors, lab99, asstch, sparkapp, sparkcsv)

Test / parallelExecution := false

javaOptions ++= Seq("-Xms512M", "-Xmx2048M", "-XX:+CMSClassUnloadingEnabled")