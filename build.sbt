
// factor out common settings into a sequence
lazy val commonSettings = Seq(
    organization := "org.hirosezouen",
    version      := "1.0.0",
    scalaVersion := "2.11.6"
)

lazy val root = (project in file(".")).
    settings(commonSettings: _*).
    settings(
        // set the name of the project
        name := "BinaryTree_test",

        // Actor of Ver2.10.0 requires to add libraryDependencies explicitly
//        libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-actors" % _ },

        // Reflect of Ver2.10.0 requires to add libraryDependencies explicitly
//        libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-reflect" % _ },

        // add ScalaTest dependency
        //libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
        //libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "compile,test",

        // add Logback, SLF4j dependencies
        libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.2",
        libraryDependencies += "ch.qos.logback" % "logback-core" % "1.1.2",
        libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.12",

        // add HZUtil dependency
        libraryDependencies += "org.hirosezouen" %% "hzutil" % "2.0.0",

        // add compile exclude files
        excludeFilter := HiddenFileFilter || "BinaryTree.scala" || "ArrayHeap.scala",

        // fork new JVM when run and test and use JVM options
//        fork := true,
//        javaOptions += "-Djava.library.path=lib",

        // misc...
        parallelExecution in Test := false,
//        logLevel := Level.Debug,
        scalacOptions += "-deprecation",
        scalacOptions += "-feature"
    )

