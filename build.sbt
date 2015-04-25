import sbt._
import Keys._

lazy val nonempty = (project in file(".")).
  settings(
    name := "non-empty",
    version := "0.0.1",
    scalaVersion := "2.11.6",

    // Compilation
    scalacOptions ++= Seq(
      "-unchecked",
      "-deprecation",
      "-feature"
    )
  )
