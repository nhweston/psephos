ThisBuild / organization := "com.github.nhweston"
ThisBuild / scalaVersion := "2.13.4"
ThisBuild / scalacOptions := Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:implicitConversions",
)

lazy val root = project.in(file(".")).settings(
  name := "psephos",
  libraryDependencies ++= Seq(),
)
