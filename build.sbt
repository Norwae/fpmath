

name := "fpmath"

organization := "com.github.norwae"

version := "1.0.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

scalaVersion := "2.12.5"