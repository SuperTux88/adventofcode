name := "adventofcode"

version := "6.0"

scalaVersion := "3.1.0"
scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-explain",
  "-explain-types",
)

assembly / mainClass := Some("adventofcode.Main")

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
libraryDependencies += "org.scala-lang.modules" % "scala-jline" % "2.12.1"

// used for json solution in day 12 2015
//libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
