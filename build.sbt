name := "adventofcode"

version := "4.0"

scalaVersion := "2.13.1"

mainClass in assembly := Some("adventofcode.Main")

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"

// used for json solution in day 12 2015
//libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
