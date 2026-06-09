name := "Scala 99"

version := "1.0"

// Scala 3 — inherits scalacOptions from root build.sbt
// Note: scalactic is a transitive dependency of scalatest and does not need
// to be listed separately.

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % Versions.scalatest % Test,
  "com.lihaoyi"   %% "os-lib"   % "0.11.8",
  "com.lihaoyi"   %% "requests" % "0.9.3",
  "com.lihaoyi"   %% "upickle"  % "4.4.3"
)

Test / unmanagedSourceDirectories += baseDirectory.value / "src/it/scala"