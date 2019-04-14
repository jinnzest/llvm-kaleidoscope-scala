enablePlugins(PackPlugin)

name := "llvm-kaleidoscope-scala"

version := "0.1"

scalaVersion := "2.12.8"

resolvers += "bintray-djspiewak-maven" at "https://dl.bintray.com/djspiewak/maven"

val ParsebackVersion = "0.3"

libraryDependencies += "com.codecommit" %% "parseback-core" % ParsebackVersion

libraryDependencies += "com.codecommit" %% "parseback-cats" % ParsebackVersion

libraryDependencies += "guru.nidi"%"graphviz-java"%"0.2.3"

libraryDependencies += "com.typesafe" % "config" % "1.3.2"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % Test

libraryDependencies +="org.scalatest" %% "scalatest" % "3.0.1" % Test