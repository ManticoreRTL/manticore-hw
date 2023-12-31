ThisBuild / scalaVersion     := "2.13.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "ch.epfl.vlsc"

val chiselVersion = "3.5.1"

lazy val root = (project in file("."))
  .settings(
    name := "manticore-machine",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % chiselVersion,
      "org.scala-lang.modules" %% "scala-xml" % "1.3.0",
      "com.github.scopt" %% "scopt" % "4.0.1", // cli arg parsing
      "com.google.ortools" % "ortools-java" % "9.3.10497", // for constraint solving
      "org.scalatest" %% "scalatest" % "3.2.9" % Test, // scala test
      "edu.berkeley.cs" %% "chiseltest" % "0.5.1" % Test
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-P:chiselplugin:genBundleElements",
    ),
    // increase java heap and stack size in order not to fail
    // code generation
    javaOptions ++= Seq(
      "-Xms512M",
      "-Xmx8192M",
      "-Xss32M",
      "-Xms256m"
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full),

  )
