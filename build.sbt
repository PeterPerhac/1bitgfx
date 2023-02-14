ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "1bitgfx",
  )

libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.9.1"
libraryDependencies += "com.monovore" %% "decline" % "2.2.0"
