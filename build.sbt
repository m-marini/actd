organization := "org.mmarini"

name := "actd"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.7"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
 
libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3-1"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.1"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.2"

libraryDependencies += "org.scalanlp" %% "breeze" % "0.11.2"

libraryDependencies += "com.github.nscala-time" % "nscala-time_2.11" % "1.8.0"

libraryDependencies += "io.reactivex" %% "rxscala" % "0.23.0"

libraryDependencies += "io.reactivex" % "rxswing" % "0.25.0"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.11.0-M7"

libraryDependencies += "org.deeplearning4j" % "deeplearning4j-core" % "0.4-rc3.8"

libraryDependencies += "org.nd4j" % "nd4j-x86" % "0.4-rc3.8"

libraryDependencies += "org.nd4j" % "canova-api" % "0.0.0.14"

libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.4.1" % Test

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % Test

libraryDependencies += "org.scalamock" %% "scalamock-scalatest-support" % "3.2" % Test

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.6" % Test

lazy val root = project in file(".")
