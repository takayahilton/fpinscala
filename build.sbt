name := "hello-scala"

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.4",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"
)

scalacOptions += "-feature"


initialCommands += "import OperationsParser._"