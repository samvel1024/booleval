ThisBuild / scalaVersion := "2.12.7"

ThisBuild / organization := "com.example"
ThisBuild / version := "0.1-SNAPSHOT"
ThisBuild / name := "booleval"


lazy val root = (project in file("."))
  .settings(
    name := "booleval",
    libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.10",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  )