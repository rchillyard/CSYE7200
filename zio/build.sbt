name := "HelloZio"

version := "1.0"

Compile / doc / scalacOptions ++= Seq("-implicits", "-deprecation")

lazy val zioVersion = "2.1.20"
lazy val zioTestVersion = "2.1.20"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % zioVersion,
  "dev.zio" %% "zio-http" % "3.4.0",
  "dev.zio" %% "zio-test-junit" % zioTestVersion,
  "dev.zio" %% "zio-test"          % zioTestVersion % Test,
  "dev.zio" %% "zio-test-sbt"      % zioTestVersion % Test,
  "dev.zio" %% "zio-test-magnolia" % zioTestVersion % Test,
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")