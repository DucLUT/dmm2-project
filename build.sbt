import Dependencies._

ThisBuild / scalaVersion := "2.13.12"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "dmm2-project",
    libraryDependencies ++= Seq(
      // Add your library dependencies here
      "org.json4s" %% "json4s-native" % "4.0.0",  // Example library dependency
      "com.typesafe.akka" %% "akka-actor" % "2.6.16" // Another example library dependency
    ),
    libraryDependencies += munit % Test
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
