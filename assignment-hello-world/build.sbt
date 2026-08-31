name := "HelloWorld"

version := "1.0"

// Scala 3 — inherits scalacOptions from root build.sbt

// MergeSortStackOverflow.merge is deliberately not tail-recursive, so the depth it
// reaches is the length of the list being merged, and whether it survives is decided
// entirely by the thread's stack size. Left to the platform default that is 2MB here
// and 1MB on the Linux CI runner, which is why MergeSortSpec passed locally and
// aborted on CI. Pinning it makes both of that spec's claims about the class
// reproducible: at 4m, 10,000 elements sort comfortably and 1,000,000 overflow.
Test / javaOptions += "-Xss4m"

libraryDependencies ++= Seq(
  "com.phasmidsoftware" %% "flog"      % Versions.flog,
  "io.circe"      %% "circe-core"      % Versions.circe,
  "io.circe"      %% "circe-generic"   % Versions.circe,
  "io.circe"      %% "circe-parser"    % Versions.circe,
  "com.lihaoyi"   %% "requests"        % Versions.requests,
  "ch.qos.logback" % "logback-classic" % Versions.logback % Runtime,
  "org.scalatest" %% "scalatest"       % Versions.scalatest % Test
)
