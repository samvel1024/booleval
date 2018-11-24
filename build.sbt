ThisBuild / scalaVersion := "2.12.7"

ThisBuild / organization := "com.example"
ThisBuild / version := "0.1-SNAPSHOT"
ThisBuild / name := "booleval"

lazy val core = (project in file("core"))
  .settings(
    name := "core",
    libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.10",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  )

lazy val web = (project in file("web"))
  .dependsOn(core)


lazy val root = (project in file("."))
  .aggregate(core, web)