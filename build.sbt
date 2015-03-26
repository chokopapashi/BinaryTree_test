name := "BinaryTree_test"

version := "1.0.0"

organization := "org.hirosezouen"

scalaVersion := "2.11.6"

// Actor of Ver2.10.1-> requires to add libraryDependencies explicitly
libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-actors" % _ }

// Reflect of Ver2.10.1-> requires to add libraryDependencies explicitly
libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-reflect" % _ }

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.2"

libraryDependencies += "ch.qos.logback" % "logback-core" % "1.1.2"

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.10"

libraryDependencies += "org.hirosezouen" %% "hzutil" % "2.0.0"

excludeFilter := HiddenFileFilter || "BinaryTree.scala" || "ArrayHeap.scala"

parallelExecution in Test := false

//scalacOptions += "-deprecation"

scalacOptions += "-feature"

//logLevel := Level.Debug

