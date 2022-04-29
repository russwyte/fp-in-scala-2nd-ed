ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.2"

lazy val root = (project in file("."))
  .settings(
    name := "fp-in-scala-2nd-ed",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M3" % Test
  )
