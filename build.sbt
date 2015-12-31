organization := "org.mmarini"

name := "actd"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.7"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
 
libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3-1"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.2"

libraryDependencies += "org.scalanlp" %% "breeze" % "0.11.2"

libraryDependencies += "com.github.nscala-time" % "nscala-time_2.11" % "1.8.0"

libraryDependencies += "io.reactivex" %% "rxscala" % "0.23.0"

libraryDependencies += "io.reactivex" % "rxswing" % "0.25.0"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.11.0-M7"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % Test

libraryDependencies += "org.scalamock" %% "scalamock-scalatest-support" % "3.2" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.6" % Test

lazy val root = project in file(".")

