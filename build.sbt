name := "adventofcode"

version := "6.0"

scalaVersion := "3.2.1"
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
libraryDependencies += "org.jline" % "jline" % "3.21.0"

// used for json solution in day 12 2015
//libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
