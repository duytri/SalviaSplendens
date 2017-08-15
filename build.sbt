name := "SalviaSplendens"
version := "0.1"
organization := "uit.master.thesis"
scalaVersion := "2.12.2"

fork in run := true
javaOptions += "-Xmx27g"
javaOptions += "-Xms7g"
//javaOptions += "-verbose:gc"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % "2.12.2"
)
