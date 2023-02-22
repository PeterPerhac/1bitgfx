ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "1bitgfx",
    assembly / mainClass := Some("com.perhac.utils.images.onebit.OneBitCodecApp"),
    assembly / assemblyJarName := "1bp-codec.jar"
  )

ThisBuild / scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
libraryDependencies += "com.github.pathikrit" %% "better-files"    % "3.9.1"
libraryDependencies += "com.monovore"         %% "decline"         % "2.2.0"
libraryDependencies += "org.bytedeco"          % "javacv-platform" % "1.5.8"
