name := "LabAWSCloudFormation"

version := "0.1"

scalaVersion := "2.11.9"

resolvers ++= Seq(Resolver.jcenterRepo)

libraryDependencies ++= Seq (
  "com.monsanto.arch" %% "cloud-formation-template-generator" % "3.8.0"
).map(_.force())

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"