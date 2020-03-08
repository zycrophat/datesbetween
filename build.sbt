enablePlugins(GitVersioning)

name := "datesbetween"

scalaVersion := "2.11.12"

organization := "steffan"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % Test

git.useGitDescribe := true

coverageEnabled := true