name := "CSYE7200"

version := "1.0"

scalaVersion := "2.12.10"

lazy val asstfc = project in file("assignment-functional-composition")

lazy val assthw = project in file("assignment-helloworld")

lazy val asstll = project in file("assignment-lazy")

lazy val asstmd = project in file("assignment-movie-database")

lazy val asstrs = project in file("assignment-random-state")

lazy val asstwc = project in file("assignment-web-crawler")

lazy val asstch = project in file("asst-cache")

lazy val concor = project in file("concordance")

lazy val fp = project in file("functional-programming")

lazy val lab99 = project in file("lab-99")

lazy val labparser = project in file("lab-parser")

lazy val labsort = project in file("lab-sorted")

lazy val mapred = project in file("mapreduce")

lazy val num = project in file("numerics")

lazy val root = (project in file(".")).aggregate(assthw, asstmd, asstrs, asstfc, asstwc, fp, num,  mapred, concor, labsort, labparser, lab99, asstch)

parallelExecution in Test := false

javaOptions ++= Seq("-Xms512M", "-Xmx2048M", "-XX:+CMSClassUnloadingEnabled")
