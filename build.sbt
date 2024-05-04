import Dependencies._

ThisBuild / scalaVersion := "2.13.12"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "dmm2-project",
    libraryDependencies ++= Seq(
      "org.json4s" %% "json4s-native" % "4.0.0",
      "com.typesafe.akka" %% "akka-actor" % "2.6.16",
      "com.github.tototoshi" %% "scala-csv" % "1.3.8"
    ),
    libraryDependencies += munit % Test
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
