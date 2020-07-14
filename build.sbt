enablePlugins(GitVersioning)

name := "datesbetween"

scalaVersion := "2.11.12"

organization := "steffan"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % Test

git.useGitDescribe := true

ThisBuild / coverageEnabled.in(Test, test) := true
ThisBuild / coverageEnabled in(Compile, compile) := false
