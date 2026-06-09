name := "Zio"
// Scala 3 — inherits scalacOptions from root build.sbt

version := "1.0"

libraryDependencies ++= Seq(
  "dev.zio"       %% "zio"               % Versions.zio,
  "dev.zio"       %% "zio-http"          % Versions.zioHttp,
  "dev.zio"       %% "zio-test-junit"    % Versions.zio,
  "dev.zio"       %% "zio-test"          % Versions.zio     % Test,
  "dev.zio"       %% "zio-test-sbt"      % Versions.zio     % Test,
  "dev.zio"       %% "zio-test-magnolia" % Versions.zio     % Test,
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")