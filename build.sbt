name := "adventofcode"

version := "8.0"

scalaVersion := "3.5.2"
scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-explain",
  "-explain-types",
)

assembly / mainClass := Some("adventofcode.Main")

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

// used for interactive version of day 13 2019
libraryDependencies += "org.jline" % "jline" % "3.24.1"
