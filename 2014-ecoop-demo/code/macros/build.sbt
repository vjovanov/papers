name := "Test-Project"

version := "1.0"

organization := "lamp.epfl.ch"

scalaVersion := "2.10.3"

resolvers += Resolver.sonatypeRepo("snapshots")

addCompilerPlugin("org.scala-lang.plugins" % "macro-paradise" % "2.0.0-SNAPSHOT" cross CrossVersion.full)

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.10.3"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.10.3"
