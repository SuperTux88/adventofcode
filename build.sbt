name := "adventofcode"

version := "5.0"

scalaVersion := "2.13.6"
scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-Wunused:imports,patvars,locals,params"
)

mainClass in assembly := Some("adventofcode.Main")

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.3"
libraryDependencies += "org.scala-lang.modules" % "scala-jline" % "2.12.1"

// used for json solution in day 12 2015
//libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
