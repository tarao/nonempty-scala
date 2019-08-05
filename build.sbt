import sbt._
import Keys._

lazy val nonempty = (project in file(".")).
  settings(
    name := "nonempty",
    organization := "com.github.tarao",
    version := "0.1.0",
    scalaVersion := "2.13.0",
    crossScalaVersions := Seq("2.13.0"),

    // Depenency
    libraryDependencies ++= Seq(
      "eu.timepit"    %% "refined"   % "0.9.9",
      "org.scalatest" %% "scalatest" % "3.0.8" % "test"
    ),

    // Compilation
    scalacOptions ++= Seq(
      "-unchecked",
      "-deprecation",
      "-feature"
    ),

    scalacOptions ++= Seq(
      "-opt:l:inline",
      "-opt-inline-from", "com.github.tarao.nonempty.**",
    ),

    // Documentation
    scalacOptions in (Compile, doc) ++= Seq(
      "-sourcepath", baseDirectory.value.getAbsolutePath,
      "-doc-source-url", "https://github.com/tarao/nonempty-scala/blob/master€{FILE_PATH}.scala",
      "-implicits",
      "-groups"
    ),

    // Publishing
    publishMavenStyle := true,
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },
    pomExtra := (
      <url>https://github.com/tarao/nonempty-scala</url>
      <licenses>
        <license>
          <name>MIT License</name>
          <url>http://www.opensource.org/licenses/mit-license.php</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:tarao/nonempty-scala.git</url>
        <connection>scm:git:git@github.com:tarao/nonempty-scala.git</connection>
      </scm>
      <developers>
        <developer>
          <id>tarao</id>
          <name>INA Lintaro</name>
          <url>https://github.com/tarao/</url>
        </developer>
      </developers>)
  )
